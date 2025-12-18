{-# LANGUAGE LambdaCase #-}
module IRPasses where

import           Core       (Function (Defined))
import qualified Data.Map   as M
import           Data.Maybe (mapMaybe)
import           Traverser

-- Cleans up PosMarker & GotoPos
foregoPos :: [Instruction] -> [Instruction]
foregoPos instrs = concatMap (\case
                                GotoPos p -> [Goto $ "P_" ++ show p]
                                PosMarker p l ->
                                  [ Label $ "P_" ++ show (x + dx, y)
                                  | let (x, y) = p, dx <- [0..l-1],
                                        (x + dx, y) `elem` gotos ]
                                PushFnVal args is -> [PushFnVal args $ foregoPos is]
                                i -> [i]) instrs
  where
    gotos = mapMaybe (\case
                       GotoPos p -> Just p
                       _ -> Nothing) instrs

doPasses :: M.Map String Function -> M.Map String Function
doPasses = M.map (\case
                     Defined args is -> Defined args (foregoPos is)
                     x -> x)
