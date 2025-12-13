module Traverser where

import           Control.Monad (void)
import           Data.List     (find)
import           Data.Maybe    (isJust, fromJust, maybeToList)
import qualified Data.Set      as S
import           Parser        (Item (..), ItemValue (..))
import Debug.Trace (trace)

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
                 | PushStr String
                 | PushChar Char
                 | Label String
                 | PosMarker Pos Int -- Pos, length
                 | Goto String
                 | GotoPos Pos
                 | JumpTrue String
                 | Exit
                 deriving (Show, Eq)

data EmitterError = Err { posn :: Pos, what :: String }
                  deriving (Show)

newtype IREmitter a = IREmitter { runEmitter :: [Instruction] -> Either EmitterError (a, [Instruction]) }

instance Functor IREmitter where
  fmap f (IREmitter run) = IREmitter $ fmap (\(a, instrs) -> (f a, instrs)) . run

instance Applicative IREmitter where
  pure a = IREmitter $ \instrs -> Right (a, instrs)
  (IREmitter runf) <*> (IREmitter runa) = IREmitter $ \instrs ->
    case runf instrs of
      Left e -> Left e
      Right (f, instrs') ->
        case runa instrs' of
          Left e              -> Left e
          Right (a, instrs'') -> pure (f a, instrs'')

instance Monad IREmitter where
  return = pure
  (IREmitter run) >>= f = IREmitter $ \instrs ->
    case run instrs of
      Left e -> Left e
      Right (a, instrs') ->
        let IREmitter run' = f a
        in run' instrs'

emit :: Instruction -> IREmitter ()
emit instr = IREmitter $ \instrs -> Right ((), instrs ++ [instr])

throwEmit :: EmitterError -> IREmitter ()
throwEmit e = IREmitter $ \_ -> Left e

showIR :: IREmitter () -> String
showIR m =
  case runEmitter m [] of
    Left e            -> show e
    Right (_, instrs) -> unlines $ map show instrs

branches :: Grid -> Pos -> Direction -> [(Direction, Pos)]
branches grid (x, y) dir
  | fst dir /= 0 = concat [
      maybeToList $ do
          Item { val = DirUp } <- grid ! (x, y - 1)
          return ((0, -1), (x, y - 1))
      , maybeToList $ do
          Item { val = DirDown } <- grid ! (x, y + 1)
          return ((0, 1), (x, y + 1))
      ]
  | snd dir /= 0 = concat [
      maybeToList $ do
          Item { val = DirLeft } <- grid ! (x - 1, y)
          return ((-1, 0), (x - 1, y))
      , maybeToList $ do
          Item { val = DirRight } <- grid ! (x + 1, y)
          return ((1, 0), (x + 1, y))
      ]
  | otherwise = error "Invalid direction"

traverse :: Grid -> Pos -> IREmitter ()
traverse grid initPos = void $ go initPos (1, 0) S.empty
  where
    go :: Pos -> Direction -> S.Set Pos -> IREmitter (S.Set Pos)
    go pos dir emitted
      | pos `S.member` emitted = do
          let pos' = pos `move` (if fst dir /= 0 then len item .* dir else dir)
              item = fromJust $ grid ! pos -- Must succeed since we met this pos
            in if val item == Space 
               then go pos' dir emitted
               else do
                 emit $ GotoPos pos
                 return emitted
      | otherwise =
        case grid ! pos of
          Nothing -> do
            let Grid g = grid
              in if fst dir == 0 && snd pos < length g -- We keep going vertically if empty line
                                                       -- This helps avoid confusion
                 then go (pos `move` dir) dir emitted
                 else do
                   emit $ PosMarker pos 1
                   emit Exit
                   return emitted
          Just item -> emit (PosMarker pos $ len item) >>
            case val item of
              Space -> go pos' dir emitted'
              DirLeft -> go (pos `move` (-1, 0)) (-1, 0) emitted'
              DirRight -> go (pos `move` (1, 0)) (1, 0) emitted'
              DirUp -> go (pos `move` (0, -1)) (0, -1) emitted'
              DirDown -> go (pos `move` (0, 1)) (0, 1) emitted'
              ItNum n -> do
                emit $ PushNum n
                go pos' dir emitted'
              StrLit s -> do
                emit $ PushStr s
                go pos' dir emitted'
              CharLit c -> do
                emit $ PushChar c
                go pos' dir emitted'
              Sym s -> do
                emit $ Call s
                go pos' dir emitted'
              Branch ->
                case branches grid pos dir of
                  [(dir', success)] -> do
                    let branchLabel = "B_" ++ show pos
                    emit $ JumpTrue branchLabel
                    emittedFail <- go pos' dir emitted'
                    emit $ Label branchLabel
                    emittedSuc <- go success dir' emitted'
                    return $ emittedFail `S.union` emittedSuc
                  bs -> trace (show bs) $ do
                    throwEmit $ Err { posn = pos, what = "Branch expects 1 outgoing direction, found " ++ show (length bs) }
                    return emitted'
              LineEnd -> error "LineEnd unreachable at traversal"
            where
              emitted' = emitted `S.union` S.fromList [(x + dx, y) | let (x, y) = pos, dx <- [0..len item - 1]]
              pos' = pos `move` (if fst dir /= 0 then len item .* dir else dir)
