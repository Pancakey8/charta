module StdLib where

import           Core      (FuncTable)
import qualified Data.Map  as M
import           StdLib.IO as IO

stdTable :: M.Map String FuncTable
stdTable = M.fromList [
  ("io", IO.table)
  ]
