module StdLib where

import StdLib.IO as IO
import qualified Data.Map as M
import Core (FuncTable)

stdTable :: M.Map String FuncTable
stdTable = M.fromList [
  ("io", IO.table)
  ]
