module Analyzer where

import Traverser (Instruction (..))
import qualified Data.Map as M
import Core (FuncTable, Function (..))
import qualified Data.Vector as V
import Debug.Trace (trace)
import Data.List (isPrefixOf)
import Data.Foldable (find, forM_)
import Parser (Arguments (..))
import Control.Monad (foldM)

data Type = TInt
          | TFloat
          | TGeneric String
          deriving (Show, Eq)

allTypes :: [Type]
allTypes = [TInt, TFloat]

type Mapping = ([Type], [Type])
newtype Behaviour = Bhv [Mapping]
                  deriving (Show, Eq)

arithmeticBehav :: [Mapping]
arithmeticBehav =
  [
    ([TInt, TInt], [TInt]),
    ([TFloat, TInt], [TFloat]),
    ([TInt, TFloat], [TFloat]),
    ([TFloat, TFloat], [TFloat])
  ]

internals :: M.Map String Behaviour
internals = M.fromList $ concatMap (\(names, maps) -> [ (name, Bhv maps) | name <- names ]) $
  [
    (["+"], arithmeticBehav),
    (["-"], arithmeticBehav),
    (["*"], arithmeticBehav),
    (["/"], arithmeticBehav),
    (["↕", "swp"], [([TGeneric "a", TGeneric "b"], [TGeneric "b", TGeneric "a"])])
  ]

collapseMap :: String -> Type -> Mapping -> Mapping
collapseMap gen t (ins, outs) = (map fix ins, map fix outs)
  where
    fix (TGeneric g) = if g == gen
                       then t
                       else TGeneric g
    fix a = a

collapseBhv :: String -> Type -> Behaviour -> Behaviour
collapseBhv gen t (Bhv ms) = Bhv $ map (collapseMap gen t) ms

allInstances :: [Type] -> [[Type]]
allInstances ts = go ts M.empty
  where
    go [] _ = [[]]
    go (TGeneric a:ts) tbl
      | a `M.member` tbl = map (tbl M.! a:) $ go ts tbl
      | otherwise = concatMap (\t -> map (t:) $ go ts $ M.insert a t tbl) allTypes
    go (t:ts) tbl = map (t:) $ go ts tbl

tryResolve :: [Mapping] -> Mapping -> [Mapping]
tryResolve maps (inp, out) = -- trace ("Trying " ++ show inp ++ "->" ++ show out) $
  case find (\(inp', _) -> inp' `isPrefixOf` out) maps of
    Nothing -> []
    Just (inp', out') -> [(inp, out' ++ drop (length inp') out)]

resolveBody :: M.Map String Behaviour -> [Instruction] -> [Type] -> Maybe Behaviour
resolveBody known is initial = go is $ zip allInsts allInsts
  where
    allInsts = allInstances initial
    go :: [Instruction] -> [Mapping] -> Maybe Behaviour
    go (i:is) states = trace ("On: " ++ show i) $ 
      case i of
        PushInt _ -> go is $ map (\(inp, out) -> (inp, TInt:out)) states
        PushFloat _ -> go is $ map (\(inp, out) -> (inp, TFloat:out)) states
        Call f ->
          if f `M.member` known
          then
            let (Bhv maps) = known M.! f
            in {-trace ("Maps: " ++ show maps) $-} 
              let states' = concatMap (tryResolve maps) states
              in go is states'
          else Nothing
        Exit -> pure $ Bhv states
    go _ _ = error "Function ended without 'Exit'"

generics :: Int -> [Type]
generics n = [ TGeneric $ "G" ++ show i | i <- [1..n] ]

analyzeProgram :: FuncTable -> IO ()
analyzeProgram tbl = do
  resolved <- foldM (\known (name, fn) ->
                          case fn of
                            Defined (Limited n) body ->
                              case resolveBody known (V.toList body) (generics n) of
                                Just bhv -> pure $ M.insert name bhv known
                                Nothing -> do
                                  putStrLn ("Unresolved: '" ++ name ++ "'")
                                  return known
                            Defined (Ellipses n) _ -> error "TODO"
                            _ -> pure known) internals $ M.toList tbl
  forM_ (M.toList resolved) $ \(k, Bhv maps) -> do
    putStrLn $ "fn " ++ k ++ " instances:"
    forM_ maps $ \(inp, out) -> do
      putStrLn $ "  " ++ show inp ++ " -> " ++ show out
