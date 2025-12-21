{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Analyzer where

import           Control.Monad ((>=>))
import           Core          (FuncTable, arithmetic, Function (Defined))
import           Data.Either   (isLeft, lefts, rights)
import           Data.Maybe    (mapMaybe)
import           Traverser     (Instruction (..))
import qualified Data.Map as M
import qualified Data.Vector as V
import Parser (Arguments(..))

data MiniValue = MInt
               | MFloat
               | MChar
               | MBool (Maybe Bool)
               deriving (Show)

instance Eq MiniValue where
  MInt == MInt = True
  MFloat == MFloat = True
  MChar == MChar = True
  MBool Nothing == MBool Nothing = True
  MBool (Just a) == MBool (Just b) = a == b
  _ == _ = False

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

returnBhv :: Behaviour
returnBhv = liftBhv return
  
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

isInt :: Behaviour
isInt = liftBhv $
  \case
    (MInt:xs) -> pure $ MBool (Just True):MInt:xs
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
    (["¿int"], isInt),
    (["+"], arithmBehav),
    (["-"], arithmBehav),
    (["*"], arithmBehav),
    (["/"], arithmBehav),
    (["∅", "drp"], liftBhv $
                   \case
                     [] -> Left "drp: needs any"
                     (_:xs) -> pure xs)
  ]

behaviourOf :: M.Map String Behaviour -> [Instruction] -> Behaviour
behaviourOf tbl prog = go prog M.empty
  where
    go [] _ = returnBhv
    go (i:is) visited =
      case i of
        PushInt _ -> pushElem MInt `andThen` go is visited
        PushChar _ -> pushElem MChar `andThen` go is visited
        PushFloat _ -> pushElem MFloat `andThen` go is visited
        Label l ->
          Bhv $
          \maps ->
            if l `M.member` visited
            then Left "what?"
            else runBhv (go is $ M.insert l maps visited) maps
        Call f ->
          if f `M.member` tbl
          then tbl M.! f `andThen` go is visited
          else error $ "Not member: " ++ f
        JumpTrue l -> branch (go (goto l) visited) (go is visited)
        Goto l ->
          Bhv $
          \maps ->
            if l `M.member` visited
            then
              if maps == visited M.! l
              then runBhv (go is visited) maps
              else Left $ "Loop has net effect: " ++ l
            else runBhv (go is $ M.insert l maps visited) maps
        Exit -> returnBhv
    goto l =
      case break (==Label l) prog of
        (_, _:is) -> is
        (_, _) -> error $ "Label " ++ l ++ " not found"

allArgs :: Int -> [[MiniValue]]
allArgs 0 = [[]]
allArgs n = concatMap opts $ allArgs (n-1)
  where
    opts x = [MInt:x, MChar:x, MBool Nothing:x, MFloat:x]

analyzeProgram :: FuncTable -> IO ()
analyzeProgram tbl = do
  let (Defined (Limited n) body) = tbl M.! "foo"
  print $ runBhv (behaviourOf internalFns $ V.toList body) $ map idMap $ allArgs n
