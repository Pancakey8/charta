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

table :: FuncTable
table = M.fromList $ concatMap (\(names, args, fn) -> [ (name, Internal args fn) | name <- names ]) [
  (["get"], Limited 0, getCh)
  ]
