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
import Debug.Trace (trace)

data MiniValue = MInt
               | MFloat
               | MChar
               | MBool (Maybe Bool)
               | MStack (Maybe [MiniValue])
               | MFn (Maybe Behaviour)
               | MAbstract
               | MGeneric String

instance Show MiniValue where
  show MInt = "MInt"
  show MFloat = "MFloat"
  show MChar = "MChar"
  show (MStack n) = "MStack " ++ show n
  show MAbstract = "MAbstract"
  show (MBool n) = "MBool " ++ show n
  show (MFn Nothing) = "MFn <?>"
  show (MFn _) = "MFn <fn>"
  show (MGeneric s) = "MGeneric " ++ show s

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

allSubs :: [MiniValue]
allSubs = [MFloat, MChar, MBool Nothing, MStack Nothing, MAbstract, MFn Nothing]

delete :: Eq a => a -> [a] -> [a]
delete a xs = case break (==a) xs of
                (k, _:l) -> k ++ l
                _ -> xs

isType :: MiniValue -> Behaviour
isType match = Bhv $ \maps -> collectResults $ map
  (\(i, o) ->
     case o of 
       (a:xs) ->
         if a == match
         then pure [(i, MBool (Just True):a:xs)]
         else
           case a of
             MGeneric g -> pure $ (subst g match i, subst g match $ MBool (Just True):match:xs)
                           : [(subst g t i, subst g t $ MBool (Just False):t:xs) | t <- delete match allSubs]
             _ -> pure [(i, MBool (Just False):MInt:xs)]
       _ -> Left "isType: Expected any")
  maps

promote :: MiniValue -> MiniValue -> Either String MiniValue
promote MInt   MInt   = Right MInt
promote MInt   MFloat = Right MFloat
promote MFloat MInt   = Right MFloat
promote MFloat MFloat = Right MFloat
promote _      _      = Left "arithm: Expected numeric arguments"

arithmBehav :: Behaviour
arithmBehav = Bhv $ \maps ->
  collectResults $ map
  (\(i, o) ->
     case o of
       (a:b:xs) ->
         let tas = maybe [a] (const [MInt, MFloat]) $ genericName a
             tbs = maybe [b] (const [MInt, MFloat]) $ genericName b
         in sequence [ do
                         r <- promote ta tb
                         pure (sub i, sub $ r:xs)
                     | ta <- tas, tb <- tbs, let sub = maybe id (`subst` ta) (genericName a)
                                                       . maybe id (`subst` tb) (genericName b)
                     ]
       _ -> Left "arithm: Expected numeric arguments")
  maps

genericName :: MiniValue -> Maybe String
genericName (MGeneric n) = pure n
genericName _ = Nothing

subst :: String -> MiniValue -> [MiniValue] -> [MiniValue]
subst gen conc vals = map (\case
                              MGeneric g ->
                                if g == gen
                                then conc
                                else MGeneric g
                              t -> t) vals

internalFns :: M.Map String Behaviour
internalFns = M.fromList $ concatMap (\(names, behav) -> [ (name, behav) | name <- names ]) $
  [
    (["¿char"], isType MChar),
    (["¿int"], isType MInt),
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
        PushStr s -> pushElem (MStack $ pure $ replicate (length s) MChar)
                     `andThen` go is visited
        PushFn name -> pushElem (MFn $ pure $ tbl M.! name)
                       `andThen` go is visited
        PushFnVal _ _ -> error "TODO"
        ForkTo _ -> error "TODO"
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
        PosMarker _ _ -> error "Unreachable in Analysis"
        GotoPos _ -> error "Unreachable in Analysis"
    goto l =
      case break (==Label l) prog of
        (_, _:is) -> is
        (_, _) -> error $ "Label " ++ l ++ " not found"

allArgs :: Int -> [MiniValue]
allArgs n = [ MGeneric ("G" ++ show i) | i <- [1..n] ]

analyzeProgram :: FuncTable -> IO ()
analyzeProgram tbl = do
  let (Defined (Limited n) body) = tbl M.! "foo"
  print $ runBhv (behaviourOf internalFns $ V.toList body) [idMap (allArgs n)]
