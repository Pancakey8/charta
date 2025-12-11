module Parser where
import Text.Parsec.String (Parser)
import Text.Parsec
    ( char,
      digit,
      satisfy,
      option,
      sourceColumn,
      (<|>),
      getPosition,
      many1, many, lookAhead, try, string, eof, token, spaces )
import Data.Char (isSpace)
import Control.Monad (void)

data ItemValue = ItNum Double
               | DirLeft
               | DirRight
               | DirUp
               | DirDown
               | Branch
               | Sym String
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
  dir <- (char '>' >> return DirRight)
         <|> (char '<' >> return DirLeft)
         <|> (char '|' >> return DirDown)
         <|> (char '^' >> return DirUp)
         <|> (char '?' >> return Branch)
  return Item { len = 1, val = dir }

sym :: Parser Item
sym = do
  sym <- many1 $ satisfy (\c -> not (isSpace c) && c `notElem` "<>|^?{}")
  return Item { len = length sym, val = Sym sym }

space :: Parser Item
space = do
  sp <- many1 $ satisfy (\c -> isSpace c && c /= '\n')
  return Item { len = length sp, val = Space }

lineBreak :: Parser Item
lineBreak = do
  void $ char '\n'
  return Item { len = 0, val = LineEnd }

parse1 :: Parser Item
parse1 = num <|> specials <|> lineBreak <|> space <|> sym

grid :: Parser [[Item]]
grid = do
  file <- many parse1
  return $ splitLines file
  where
    tailSafe [] = []
    tailSafe (_:xs) = xs

    splitLines [] = []
    splitLines is =
      let (line, rest) = span (\item -> val item /= LineEnd) is
      in line : splitLines (tailSafe rest)

type Function = (String, [[Item]])

func :: Parser Function
func = do
  void $ string "fn"
  spaces
  Item { val = Sym s } <- sym
  spaces
  void $ string "{"
  void $ char '\n'
  body <- grid
  void $ string "}"
  return (s, body)


parseProgram :: Parser [Function]
parseProgram = do
  fns <- many $ spaces *> func <* spaces
  eof
  return fns
