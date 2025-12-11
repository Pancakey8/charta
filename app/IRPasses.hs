{-# LANGUAGE LambdaCase #-}
module IRPasses where

import Traverser
import Data.Maybe (mapMaybe)

-- Cleans up PosMarker & GotoPos
foregoPos :: [Instruction] -> [Instruction]
foregoPos instrs = concatMap (\case
                                GotoPos p -> [Goto $ "P_" ++ show p]
                                PosMarker p l ->
                                  [ Label $ "P_" ++ show (x + dx, y)
                                  | let (x, y) = p, dx <- [0..l-1],
                                        (x + dx, y) `elem` gotos ]
                                i -> [i]) instrs
  where
    gotos = mapMaybe (\case
                       GotoPos p -> Just p
                       _ -> Nothing) instrs

doPasses :: [Instruction] -> [Instruction]
doPasses = foregoPos
