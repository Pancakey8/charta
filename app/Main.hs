module Main (main) where

import Parser
import Traverser
import Text.Parsec (parse)
import IRPasses
import Data.Foldable (for_)

main :: IO ()
main = do
  res <- parse parseProgram "" <$> readFile "example.ch"
  case res of
    Left e -> print e
    Right file -> do
      for_ file $ \(name, body) -> do
        let ir = makeFunction name (Grid body) (0, 0)
        putStrLn "RAW:"
        putStrLn $ showIR ir
        case runEmitter ir [] of
          Left _ -> return ()
          Right (_, instrs) -> do
            let instrs' = doPasses instrs
            putStrLn "\nPASSES:"
            putStrLn $ unlines $ map show instrs'
        putStrLn "\n---\n"
