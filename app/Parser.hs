module Parser where
import           Control.Monad      (void, when)
import           Data.Char          (isSpace)
import           Text.Parsec        (anyChar, char, digit, eof, getPosition,
                                     lookAhead, many, many1, manyTill, option,
                                     satisfy, sourceColumn, spaces, string, try,
                                     (<|>), unexpected)
import           Text.Parsec.String (Parser)

data ItemValue = ItFloat Double
               | ItInt Int
               | DirLeft
               | DirRight
               | DirUp
               | DirDown
               | Branch
               | Fork
               | Sym String
               | StrLit String
               | CharLit Char
               | FnRef String
               | FnVal Arguments [Item]
               | Space
               | LineEnd
               deriving (Show, Eq)

data Item = Item { len :: Int, val :: ItemValue }
          deriving (Show, Eq)

sign :: Num a => Parser (a -> a)
sign = (char '-' *> andDigit >> return negate) <|> (char '+' *> andDigit >> return id) <|> return id
  where andDigit = lookAhead digit

float :: Parser Item
float = try $ do
  start <- getPosition
  sgn <- sign
  int <- many1 digit
  frac <- char '.' >> many1 digit >>= return . ('.':)
  end <- getPosition
  return Item { len = sourceColumn end - sourceColumn start, val = ItFloat (sgn $ read (int ++ frac)) }

int :: Parser Item
int = try $ do
  start <- getPosition
  sgn <- sign
  int <- many1 digit
  end <- getPosition
  return Item { len = sourceColumn end - sourceColumn start, val = ItInt (sgn $ read int) }

num :: Parser Item
num = try float <|> try int 

specials :: Parser Item
specials = do
  dir <- (char '→' >> return DirRight) -- \leftarrow
         <|> (char '←' >> return DirLeft) -- \rightarrow
         <|> (char '↓' >> return DirDown) -- \downarrow
         <|> (char '↑' >> return DirUp) -- \uparrow
         <|> (char '?' >> return Branch)
         <|> (char '⋔' >> return Fork)
  return Item { len = 1, val = dir }

sym :: Parser Item
sym = do
  symStr <- many1 $ satisfy (\c -> not (isSpace c) && c `notElem` "←→↑↓?⋔{}()\"#[]")
  return Item { len = length symStr, val = Sym symStr }

space :: Parser Item
space = do
  sp <- many1 $ satisfy (\c -> isSpace c && c /= '\n')
  return Item { len = length sp, val = Space }

comment :: Parser Item
comment = do
  start <- getPosition
  void $ string "{#"
  void $ manyTill (satisfy (`notElem` "\r\n")) $ string "#}"
  end <- getPosition
  return Item { len = sourceColumn end - sourceColumn start, val = Space }

escape :: Char -> Char
escape 'n' = '\n'
escape 't' = '\t'
escape c   = c

strLit :: Parser Item
strLit = do
  start <- getPosition
  void $ char '"'
  s <- manyTill charP $ char '"'
  end <- getPosition
  return Item { len = sourceColumn end - sourceColumn start, val = StrLit s }

charP :: Parser Char
charP = satisfy (`notElem` "\r\n") >>= \c -> if c == '\\'
                                             then escape <$> anyChar
                                             else return c

charLit :: Parser Item
charLit = do
  start <- getPosition
  void $ char '#'
  c <- charP
  end <- getPosition
  return Item { len = sourceColumn end - sourceColumn start, val = CharLit c }

fnRef :: Parser Item
fnRef = do
  start <- getPosition
  void $ char '['
  Item { val = Sym s } <- sym
  void $ char ']'
  end <- getPosition
  return Item { len = sourceColumn end - sourceColumn start, val = FnRef s }

fnVal :: Parser Item
fnVal = do
  start <- getPosition
  void $ char '['
  spaces
  void $ char '('
  args <- many $ spaces *> sym <* spaces
  void $ char ')'
  spaces
  g <- grid
  void $ char ']'
  end <- getPosition
  when (length g > 1) $ unexpected "Multiline function values not allowed"
  case lastMaybe args of
    Just (Item { val = Sym "..." }) -> return Item { len = sourceColumn end - sourceColumn start, val = FnVal (Ellipses $ length args - 1) (head g) }
    _ -> return Item { len = sourceColumn end - sourceColumn start, val = FnVal (Limited $ length args) (head g) }
  

lineBreak :: Parser Item
lineBreak = do
  void $ char '\n'
  return Item { len = 0, val = LineEnd }

parse1 :: Parser Item
parse1 = num <|> strLit <|> try fnRef <|> fnVal <|> charLit <|> specials <|> lineBreak <|> comment <|> space <|> sym

grid :: Parser [[Item]]
grid = do
  file <- many parse1
  return $ splitLines file
  where
    tailSafe []     = []
    tailSafe (_:xs) = xs

    splitLines [] = []
    splitLines is =
      let (line, rest) = span (\item -> val item /= LineEnd) is
      in line : splitLines (tailSafe rest)

data Arguments = Limited Int
               | Ellipses Int
               deriving (Show, Eq)
data Visibility = Visible | Hidden
                deriving (Show, Eq)
type Function = (String, Arguments, [[Item]], Visibility)

data TopLevel = FuncDecl Function
              | UseDrv String (Maybe String)
              | FFIDecl FFIFunc

lastMaybe :: [a] -> Maybe a
lastMaybe []     = Nothing
lastMaybe [x]    = pure x
lastMaybe (x:xs) = lastMaybe xs

type FFIFunc = (String, String, Int, String)

ffiFunc :: Parser FFIFunc
ffiFunc = do
  void $ string "ffi"
  spaces
  Item { val = StrLit lib } <- strLit
  spaces
  void $ string "fn"
  spaces
  Item { val = Sym name } <- sym
  spaces
  void $ char '('
  args <- many $ spaces *> sym <* spaces
  void $ char ')'
  spaces
  void $ string "->"
  spaces
  Item { val = Sym ret } <- sym
  spaces
  return (lib, name, length args, ret)

func :: Parser Function
func = do
  priv <- option Visible $ string "hide" >> pure Hidden
  spaces
  void $ string "fn"
  spaces
  Item { val = Sym s } <- sym
  spaces
  void $ char '('
  args <- many $ spaces *> sym <* spaces
  void $ char ')'
  spaces
  void $ string "{"
  void $ char '\n'
  body <- grid
  void $ string "}"
  case lastMaybe args of
    Just (Item { val = Sym "..." }) -> return (s, Ellipses (length args - 1), body, priv)
    _ -> return (s, Limited (length args), body, priv)

useDrv :: Parser TopLevel
useDrv = do
  void $ string "use"
  spaces
  Item { val = StrLit s } <- strLit
  spaces
  ns <- option Nothing $ do
    void $ string "as"
    spaces
    Item { val = Sym ns' } <- sym
    return $ Just ns'
  return $ UseDrv s ns

topLevel :: Parser TopLevel
topLevel = try (FuncDecl <$> func) <|> try (FFIDecl <$> ffiFunc) <|> useDrv

parseProgram :: Parser [TopLevel]
parseProgram = do
  tls <- many $ spaces *> topLevel <* spaces
  eof
  return tls
