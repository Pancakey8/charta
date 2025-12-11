module Main (main) where
import Data.Char (isDigit)

import Parser
import Traverser
import Text.Parsec (parse)
import IRPasses

main :: IO ()
main = do
  res <- parse parseEOF "" <$> readFile "example.ch"
  case res of
    Left _ -> return ()
    Right file -> do
      let ir = Traverser.traverse (Grid file) (0, 0)
      putStrLn $ showIR ir
      case runEmitter ir [] of
        Left _ -> return ()
        Right (_, instrs) -> do
          let instrs' = doPasses instrs
          putStrLn "==="
          putStrLn $ unlines $ map show instrs'
