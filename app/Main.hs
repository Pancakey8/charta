module Main (main) where

import           Control.Monad (forM)
import           Core          (Function (Defined))
import           Data.Functor  (void)
import qualified Data.Map      as M
import           Interpreter
import           IRPasses
import           Parser
import           Text.Parsec   (parse)
import           Traverser

main :: IO ()
main = do
  res <- parse parseProgram "" <$> readFile "example.ch"
  case res of
    Left e -> print e
    Right file -> do
      fns <- forM file $ \(name, body) -> do
        let ir = Traverser.traverse (Grid body) (0, 0)
        putStrLn "RAW:"
        putStrLn $ "fn " ++ name
        putStrLn $ showIR ir
        case runEmitter ir [] of
          Left e -> do
            print e
            return Nothing
          Right (_, instrs) -> do
            let instrs' = doPasses instrs
            putStrLn "\nPASSES:"
            putStrLn $ "fn " ++ name
            putStrLn $ unlines $ map show instrs'
            putStrLn "\n---\n"
            return $ Just (name, instrs')
      case sequence fns of
        Nothing -> return ()
        Just fns' -> void $ runProgram $ M.fromList $
                     map (\(name, f) -> (name, Defined f)) fns'
