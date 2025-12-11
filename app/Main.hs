module Main (main) where
import Data.Char (isDigit)

import Parser
import Traverser
import Text.Parsec (parse)

main :: IO ()
main = do
  res <- parse parseEOF "" <$> readFile "example.ch"
  case res of
    Left _ -> return ()
    Right file ->
      putStrLn $ showIR $ Traverser.traverse (Grid file) (0, 0)
