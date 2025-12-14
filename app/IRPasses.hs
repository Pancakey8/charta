{-# LANGUAGE LambdaCase #-}
module IRPasses where

import           Data.Maybe (mapMaybe)
import           Traverser
import qualified Data.Map as M
import Parser (Arguments)

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

--                Function name -> Arguments, instructions
type Program = M.Map String (Arguments, [Instruction])
--                     local -> canonical
data BuildUnit = Unit (M.Map String String) Program

-- TODO: Bring back a doPasses function to handle everything:
canonicalizeNames :: BuildUnit -> Program
canonicalizeNames (Unit names fns) = M.foldrWithKey resolve fns names
  where
    resolve local canon tbl =
      let tbl' = M.insert canon (tbl M.! local) $ M.delete local tbl
      in M.map (\(argc, body) -> (argc, resolveBody local canon body)) tbl'
    resolveBody _ _ [] = []
    resolveBody local canon (i:is) = rewrite local canon i : resolveBody local canon is
    rewrite local canon (Call c) = if c == local then Call canon else Call c
    rewrite local canon (PushFn c) = if c == local then PushFn canon else PushFn c
    rewrite _ _ x = x

