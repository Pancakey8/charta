{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Analyzer where

import           Control.Monad ((>=>))
import           Core          (FuncTable, arithmetic)
import           Data.Either   (isLeft, lefts, rights)
import           Data.Maybe    (mapMaybe)
import           Traverser     (Instruction (..))
import qualified Data.Map as M

data MiniValue = MInt
               | MFloat
               | MChar
               | MBool (Maybe Bool)
               deriving (Show)

type Mapping = ([MiniValue], [MiniValue])

idMap :: [MiniValue] -> Mapping
idMap args = (args, args)

newtype Behaviour = Bhv { runBhv :: [Mapping] -> Either String [Mapping] }

collectResults :: [Either e [a]] -> Either e [a]
collectResults xs =
  let
    rs = concat (rights xs)
    ls = lefts xs
  in
    case rs of
      [] -> case ls of
              (e:_) -> Left e
              []    -> Right []
      _ -> Right rs

liftBhv :: ([MiniValue] -> Either String [MiniValue]) -> Behaviour
liftBhv f = Bhv $ \maps -> collectResults $ [ fmap (\outs' -> [(ins, outs')]) (f outs) | (ins, outs) <- maps ]

andThen :: Behaviour -> Behaviour -> Behaviour
(Bhv f) `andThen` (Bhv g) = Bhv (f >=> g)

pushElem :: MiniValue -> Behaviour
pushElem x = liftBhv $ \stk -> pure $ x:stk

branch :: Behaviour -> Behaviour -> Behaviour
branch success fail = Bhv $ \maps ->
  collectResults (map branchOne maps)
  where
    branchOne (i, o) =
      case o of
        MBool (Just True):xs -> runBhv success [(i, xs)]
        MBool (Just False):xs -> runBhv fail [(i, xs)]
        MBool Nothing:xs ->
          liftA2 (++) (runBhv success [(i, xs)]) (runBhv fail [(i, xs)])
        _ ->
          Left "branch: Expected bool"

isChar :: Behaviour
isChar = liftBhv $
  \case
    (MChar:xs) -> pure $ MBool (Just True):MChar:xs
    s@(_:xs) -> pure $ MBool (Just False):s
    _ -> Left "isChar: Expected any"

arithmBehav :: Behaviour
arithmBehav = liftBhv $
  \case
    (MInt:MInt:xs) -> pure $ MInt:xs
    (MFloat:MInt:xs) -> pure $ MFloat:xs
    (MInt:MFloat:xs) -> pure $ MFloat:xs
    (MFloat:MFloat:xs) -> pure $ MFloat:xs
    _ -> Left "arithmetic: Expected numbers"

internalFns :: M.Map String Behaviour
internalFns = M.fromList $ concatMap (\(names, behav) -> [ (name, behav) | name <- names ]) $
  [
    (["¿char"], isChar),
    (["+"], arithmBehav),
    (["-"], arithmBehav),
    (["*"], arithmBehav),
    (["/"], arithmBehav)
  ]

analyzeProgram :: FuncTable -> IO ()
analyzeProgram = error "TODO"
