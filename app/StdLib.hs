module StdLib where

import           Core      (FuncTable)
import qualified Data.Map  as M
import           StdLib.IO as IO
import           StdLib.FS as FS

stdTable :: M.Map String FuncTable
stdTable = M.fromList [
  ("io", IO.table),
  ("strings", M.empty),
  ("fs", FS.table)
  ]
