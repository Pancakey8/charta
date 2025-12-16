module Main (main) where

import           Control.Monad      (forM_, void, when)
import           Core               (Function (..))
import qualified Data.Map           as M
import           Interpreter        (runProgram)
import           IRPasses
import           ProgramBuilder
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      putStrLn "Usage: charta <source-file.ch> [-src,-ir]"
      return ()
    else do
      let opts = tail args
      src <- buildSource $ head args
      when ("-src" `elem` opts) $ printSourceTree src
      let table = flattenTree src
          table' = doPasses table
      when ("-ir" `elem` opts) $ mapM_ display (M.toList table')
      void $ runProgram table'
  where
    display (name, Defined args instrs) = do
      putStrLn $ "fn " ++ name ++ "(" ++ show args ++ "):"
      forM_ instrs $ \instr -> do
        putStrLn $ "  " ++ show instr
    display (name, Internal args _) = do
      putStrLn $ "fn " ++ name ++ "(" ++ show args ++ "):"
      putStrLn "  <internal>"
    display (name, Mixed args _) = do
      putStrLn $ "fn " ++ name ++ "(" ++ show args ++ "):"
      putStrLn "  <internal>"
