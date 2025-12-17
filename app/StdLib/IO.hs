module StdLib.IO where
import           Core      (FuncTable, Function (..), Value (..), stringified)
import qualified Data.Map  as M
import           Parser    (Arguments (..))
import           System.IO (BufferMode (NoBuffering), hGetChar, hSetBuffering,
                            stdin)

getCh :: [Value] -> IO [Value]
getCh vs = do
  hSetBuffering stdin NoBuffering
  c <- getChar
  return $ ValChar c:vs

printVal :: [Value] -> IO [Value]
printVal (v:vs) = putStr (stringified v) >> return vs

table :: FuncTable
table = M.fromList $ concatMap (\(names, args, fn) -> [ (name, Internal args fn) | name <- names ]) [
  (["∈", "get"], Limited 0, getCh), -- in
  (["∋", "print"], Limited 1, printVal) -- ni
  ]
