{-# LANGUAGE TupleSections #-}
module Traverser where

import Parser (Item(..), ItemValue (..))
import qualified Data.Set as S
import Debug.Trace (trace)
import Data.List (find)

newtype Grid = Grid [[Item]]
             deriving (Show)
type Pos = (Int, Int)
type Direction = (Int, Int)

(!) :: Grid -> Pos -> Maybe Item
(!) (Grid g) (x, y)
  | y < 0 || y >= length g
    || x < 0 || x >= (sum . map len $ g !! y) = Nothing
  | otherwise = let row = g !! y
                    sums = scanl (\acc i -> acc + len i) 0 row
                in fmap snd $ find
                   (\(acc, it) -> acc <= x && x < (acc + len it)) $ zip sums row

move :: Pos -> Direction -> Pos
move (x, y) (dx, dy) = (x + dx, y + dy)

(.*) :: Int -> (Int, Int) -> (Int, Int)
c .* (x, y) = (c * x, c * y)

data Instruction = Call String
                 | PushNum Double
                 | Label String
                 | Goto String
                 deriving (Show)

labelAt :: Pos -> String
labelAt (x,y) = "L_" ++ show x ++ "_" ++ show y

newtype IREmitter a = IREmitter { runEmitter :: [Instruction] -> (a, [Instruction]) }

instance Functor IREmitter where
  fmap f (IREmitter run) = IREmitter $ \instrs ->
    let (a, instrs') = run instrs
    in (f a, instrs')

instance Applicative IREmitter where
  pure a = IREmitter (a,) 
  (<*>) (IREmitter runf) (IREmitter runa) = IREmitter $ \instrs ->
    let (f, instrs') = runf instrs
        (a, instrs'') = runa instrs'
    in (f a, instrs'')

instance Monad IREmitter where
  return = pure
  (>>=) (IREmitter run) f = IREmitter $ \instrs ->
    let (a, instrs') = run instrs
        IREmitter run' = f a
    in run' instrs'

emit :: Instruction -> IREmitter ()
emit instr = IREmitter $ \instrs -> ((), instrs ++ [instr])

showIR :: IREmitter () -> String
showIR m = 
  let (_, instrs) = runEmitter m []
  in unlines $ map show instrs

traverse :: Grid -> Pos -> IREmitter ()
traverse grid initPos = trace (show grid) $ go initPos (1, 0) S.empty
  where
    go :: Pos -> Direction -> S.Set Pos -> IREmitter ()
    go pos dir emitted
      | pos `S.member` emitted = emit $ Goto $ labelAt pos
      | otherwise =
        trace ("Emitting " ++ show pos) $
        case grid ! pos of
          Nothing -> return ()
          Just item ->
            case val item of
              Space -> go pos' dir emitted'
              DirLeft -> go (pos `move` (-1, 0)) (-1, 0) emitted'
              DirRight -> go (pos `move` (1, 0)) (1, 0) emitted'
              DirUp -> go (pos `move` (0, -1)) (0, -1) emitted'
              DirDown -> go (pos `move` (0, 1)) (0, 1) emitted'
              ItNum n -> do
                emit $ Label $ labelAt pos
                emit $ PushNum n
                go pos' dir emitted'
              Sym s -> do
                emit $ Label $ labelAt pos
                emit $ Call s
                go pos' dir emitted'
              Branch -> error "TODO: Branching"
              LineEnd -> error "LineEnd unreachable at traversal"
            where
              emitted' = S.insert pos emitted
              pos' = pos `move` (if fst dir /= 0 then len item .* dir else dir)
