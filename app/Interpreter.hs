module Interpreter where

import qualified Data.Map   as M

import           Core
import           Data.List  (find)
import           Data.Maybe (fromJust)
import           Traverser  (Instruction (..))
import Debug.Trace (trace)

data Frame = Frame { prog :: [Instruction], pc :: Int, stack :: [Value] }
           deriving (Show)

data Context = Ctx { frames :: [Frame], fns :: FuncTable }
             deriving (Show)

run :: Context -> IO Context
run ctx = -- trace (show ctx) $
  case frames ctx of
    [] -> return ctx
    f:fs ->
      let instr = prog f !! pc f
      in if instr == Exit
         then -- trace (show f) $
           case fs of
             f':fs' -> run ctx { frames = f' { pc = pc f' + 1, stack = stack f ++ stack f' } : fs' }
             []    -> run ctx { frames = [] }
         else step instr ctx >>= run

advance :: Context -> IO Context
advance ctx =
  case frames ctx of
    f : fs -> return ctx { frames = f { pc = pc f + 1 } : fs }
    []     -> return ctx

headStack :: Context -> Value
headStack = head . stack . head . frames 

modifyStack :: Context -> ([Value] -> [Value]) -> Context
modifyStack ctx m = let f:fs = frames ctx
                    in ctx { frames = f { stack = m (stack f) } : fs }

step :: Instruction -> Context -> IO Context
step i ctx = -- trace (show $ stack ctx) $
  case i of
    Call f        -> case fns ctx M.! f of -- TODO: Handle fn not found
                       Defined argc body -> let f:fs = frames ctx -- TODO: Handle few-args errors.
                                            in return ctx { frames = Frame body 0 (take argc $ stack f)
                                                                     : f { stack = drop argc $ stack f }
                                                                     : fs }
                       Internal fn -> fn (stack $ head $ frames ctx) >>= \c -> advance $
                                                                               modifyStack ctx (const c)

    PushNum n     -> advance $ modifyStack ctx $ \stk -> ValNum n : stk

    PushStr s     -> advance $ modifyStack ctx $ \stk -> ValStr s : stk

    Label _       -> advance ctx

    Goto l        -> performGo l ctx

    JumpTrue l    -> let ctx' = modifyStack ctx $ \(top:rs) -> rs -- TODO: Handle empty stack
                         top = headStack ctx
                     in if truthy top
                        then performGo l ctx'
                        else advance ctx'

    GotoPos _     -> error "Impossible instruction hit"
    PosMarker _ _ -> error "Impossible instruction hit"
                     -- ^ due to being eliminated in IR pass
    Exit          -> error "Impossible instruction hit"
                     -- ^ due to being handled in 'run'
  where
    performGo :: String -> Context -> IO Context
    performGo label ctx' = let f:fs = frames ctx'
                               n = fst $ fromJust $ find ((==Label label) . snd) $
                                   zip [0..] (prog f)
                          in return ctx' { frames = f { pc = n } : fs }

runProgram :: FuncTable -> IO Context
runProgram table = do
  let Defined 0 main = table M.! "main" -- TODO: Handle fn not found
  run $ Ctx {
    frames = [Frame { prog = main, pc = 0, stack = [] }],
    fns = coreTable `M.union` table }
