module StdLib.FS where
import Core
import qualified Data.Map as M
import Parser (Arguments(..))
import System.IO (Handle, openFile, IOMode (ReadMode), hGetContents, hClose, hFileSize, hTell, hPutStr, hGetChar)
import Data.Dynamic (toDyn, fromDynamic)
import GHC.IO.IOMode (IOMode(..))
import GHC.Float (int2Double)

openFS :: IOMode -> [Value] -> IO [Value]
openFS mode (ValStack s:vs) =
  case maybeString s of
    Just fp -> do
      h <- openFile fp mode
      return $ ValAbstract (Abs (toDyn h)):vs
    Nothing -> error "openFS: Expected string, got stack"
openFS _ _ = error "openFS: Expected string"

slurp :: [Value] -> IO [Value]
slurp stk@(ValAbstract (Abs a):vs) =
  case fromDynamic a :: Maybe Handle of
    Just fs -> do
      str <- hGetContents fs
      return $ ValStack (map ValChar str):stk
    Nothing -> error "slurp: Expected file-stream, got abstract"
slurp _ = error "slurp: Expected file-stream"

closeFS :: [Value] -> IO [Value]
closeFS (ValAbstract (Abs a):vs) =
  case fromDynamic a :: Maybe Handle of
    Just fs -> do
      hClose fs
      return vs
    Nothing -> error "closeFS: Expected file-stream, got abstract"
closeFS _ = error "closeFS: Expected file-stream"

sizeFS :: [Value] -> IO [Value]
sizeFS stk@(ValAbstract (Abs a):vs) =
  case fromDynamic a :: Maybe Handle of
    Just fs -> do
      sz <- hFileSize fs
      return $ ValNum (fromInteger sz):stk
    Nothing -> error "sizeFS: Expected file-stream, got abstract"
sizeFS _ = error "sizeFS: Expected file-stream"

tellFS :: [Value] -> IO [Value]
tellFS stk@(ValAbstract (Abs a):vs) =
  case fromDynamic a :: Maybe Handle of
    Just fs -> do
      sz <- hTell fs
      return $ ValNum (fromInteger sz):stk
    Nothing -> error "tellFS: Expected file-stream, got abstract"
tellFS _ = error "tellFS: Expected file-stream"

printFS :: [Value] -> IO [Value]
printFS (v:stk@(ValAbstract (Abs a):vs)) =
  case fromDynamic a :: Maybe Handle of
    Just fs -> do
      hPutStr fs $ stringified v
      return stk
    Nothing -> error "printFS: Expected file-stream, got abstract"
printFS _ = error "printFS: Expected value and file-stream"

readFS :: [Value] -> IO [Value]
readFS stk@(ValAbstract (Abs a):vs) =
  case fromDynamic a :: Maybe Handle of
    Just fs -> do
      c <- hGetChar fs
      return $ ValChar c:stk
    Nothing -> error "readFS: Expected file-stream, got abstract"
readFS _ = error "readFS: Expected file-stream"

table :: FuncTable
table = M.fromList $ concatMap (\(names, args, fn) -> [ (name, Internal args fn) | name <- names ]) [
  (["⨆r", "openr"], Limited 1, openFS ReadMode), -- \bigsqcup
  (["⨆w", "openw"], Limited 1, openFS WriteMode), 
  (["⨆", "openrw"], Limited 1, openFS ReadWriteMode), 
  (["∋", "print"], Limited 2, printFS), 
  (["∈", "read"], Limited 1, readFS), 
  (["sz"], Limited 1, sizeFS),
  (["tell"], Limited 1, tellFS),
  (["×", "close"], Limited 1, closeFS), -- \times
  (["Σ", "slurp"], Limited 1, slurp) -- \Sigma
  ]
