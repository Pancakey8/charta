module Main (main) where

import           Control.Monad      (forM)
import           Core               (Function (Defined))
import           Data.Functor       (void)
import qualified Data.Map           as M
import           Interpreter        (runProgram)
import           IRPasses           (doPasses)
import           Parser             (parseProgram)
import           System.Environment (getArgs)
import           Text.Parsec        (parse)
import           Traverser          (Grid (Grid), IREmitter (runEmitter),
                                     showIR, traverse)

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then do
      putStrLn "Usage: charta <source-file.ch>"
      return ()
    else do
      res <- parse parseProgram "" <$> readFile (head args)
      case res of
          Left e -> print e
          Right file -> do
            fns <- forM file $ \(name, argc, body) -> do
              print body
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
                  return $ Just (name, argc, instrs')
            case sequence fns of
              Nothing -> return ()
              Just fns' -> void $ runProgram $ M.fromList $
                           map (\(name, argc, f) -> (name, Defined argc f)) fns'
