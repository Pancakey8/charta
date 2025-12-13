module Parser where
import           Control.Monad      (void)
import           Data.Char          (isSpace)
import           Text.Parsec        (anyChar, char, digit, eof, getPosition,
                                     lookAhead, many, many1, manyTill, option,
                                     satisfy, sourceColumn, spaces, string, try,
                                     (<|>))
import           Text.Parsec.String (Parser)

data ItemValue = ItNum Double
               | DirLeft
               | DirRight
               | DirUp
               | DirDown
               | Branch
               | Sym String
               | StrLit String
               | CharLit Char
               | FnRef String
               | Space
               | LineEnd
               deriving (Show, Eq)

data Item = Item { len :: Int, val :: ItemValue }
          deriving (Show, Eq)

num :: Parser Item
num = try $ do
  start <- getPosition
  sgn <- (char '-' *> andDigit >> return negate) <|> (char '+' *> andDigit >> return id) <|> return id
  int <- many1 digit
  frac <- option "" $ char '.' >> many1 digit >>= return . ('.':)
  end <- getPosition
  return Item { len = sourceColumn end - sourceColumn start, val = ItNum (sgn $ read (int ++ frac)) }
  where
    andDigit = lookAhead digit

specials :: Parser Item
specials = do
  dir <- (char '→' >> return DirRight) -- \leftarrow
         <|> (char '←' >> return DirLeft) -- \rightarrow
         <|> (char '↓' >> return DirDown) -- \downarrow
         <|> (char '↑' >> return DirUp) -- \uparrow
         <|> (char '?' >> return Branch)
  return Item { len = 1, val = dir }

sym :: Parser Item
sym = do
  symStr <- many1 $ satisfy (\c -> not (isSpace c) && c `notElem` "←→↑↓?{}()\"#[]")
  return Item { len = length symStr, val = Sym symStr }

space :: Parser Item
space = do
  sp <- many1 $ satisfy (\c -> isSpace c && c /= '\n')
  return Item { len = length sp, val = Space }

comment :: Parser Item
comment = do
  start <- getPosition
  void $ string "{#"
  sp <- manyTill anyChar $ string "#}"
  end <- getPosition
  return Item { len = sourceColumn end - sourceColumn start, val = Space }

escape :: Char -> Char
escape 'n' = '\n'
escape 't' = '\t'
escape c = c

strLit :: Parser Item
strLit = do
  start <- getPosition
  void $ char '"'
  s <- manyTill charP $ char '"'
  end <- getPosition
  return Item { len = sourceColumn end - sourceColumn start, val = StrLit s }

charP :: Parser Char
charP = anyChar >>= \c -> if c == '\\'
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

lineBreak :: Parser Item
lineBreak = do
  void $ char '\n'
  return Item { len = 0, val = LineEnd }

parse1 :: Parser Item
parse1 = num <|> strLit <|> fnRef <|> charLit <|> specials <|> lineBreak <|> comment <|> space <|> sym

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

type Function = (String, Int, [[Item]])

data TopLevel = FuncDecl Function
              | UseDrv String (Maybe String)

func :: Parser Function
func = do
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
  return (s, length args, body)

useDrv :: Parser TopLevel
useDrv = do
  void $ string "use"
  spaces
  Item { val = StrLit s } <- strLit
  spaces
  ns <- option Nothing $ do
    void $ string "as"
    spaces
    Item { val = Sym s } <- sym
    return $ Just s
  return $ UseDrv s ns

topLevel :: Parser TopLevel
topLevel = (FuncDecl <$> func) <|> useDrv
  
parseProgram :: Parser [TopLevel]
parseProgram = do
  tls <- many $ spaces *> topLevel <* spaces
  eof
  return tls
