{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import           Control.Monad      (foldM, forM_, void, when)
import           Core               (Function (..))
import qualified Data.Map           as M
import           FFI                (FFITable, closeTable)
import           FFI.Unix           (loadShared, loadSymbol)
import           Interpreter        (runProgram)
import           IRPasses
import           ProgramBuilder
import           System.Environment (getArgs)

loadIfForeign :: FFITable -> SourceElem -> IO (FFITable, Function)
loadIfForeign tbl (Native (_, f))                  = return (tbl, f)
loadIfForeign tbl (Foreign (libName, name, argc, rets))
  | libName `M.member` tbl = do
      ptr <- loadSymbol (tbl M.! libName) name
      return (tbl, External ptr argc rets)
  | otherwise = do
      lib <- loadShared libName
      ptr <- loadSymbol lib name
      return (M.insert libName lib tbl, External ptr argc rets)

loadOnTable :: M.Map String SourceElem -> IO (FFITable, M.Map String Function)
loadOnTable tbl = foldM step (M.empty, M.empty) $ M.toList tbl
  where
    step (ffi, funcs) (name, src) = do
      (ffi', func) <- loadIfForeign ffi src
      return (ffi', M.insert name func funcs)

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
      (ffiTbl, withForeigns) <- loadOnTable table
      let withPasses = M.map doPasses withForeigns
      when ("-ir" `elem` opts) $ mapM_ display (M.toList withPasses)
      void $ runProgram withPasses
      closeTable ffiTbl
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
