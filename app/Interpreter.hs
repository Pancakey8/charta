{-# LANGUAGE BangPatterns #-}
module Interpreter where

import qualified Data.Map                 as M

import           Control.Concurrent.Async (async)
import           Core
import           Data.Dynamic             (toDyn)
import qualified Data.IntMap              as IM
import           Data.List                (find)
import           Data.Maybe               (fromJust)
import qualified Data.Vector              as V
import           Debug.Trace              (trace)
import           FFI                      (callWith)
import           Parser                   (Arguments (..))
import           Traverser                (Instruction (..))

run :: Context -> IO Context
run ctx = -- trace (show ctx) $
  case frames ctx of
    [] -> return ctx
    f:fs ->
      let instr = prog f V.! pc f
      in if instr == Exit
         then -- trace (show f) $
           case fs of
             f':fs' -> let !stk = stack f ++ stack f' in run ctx { frames = f' { pc = pc f' + 1, stack = stk  } : fs' }
             []    -> return ctx
         else step instr ctx >>= run

advance :: Context -> IO Context
advance ctx =
  case frames ctx of
    f : fs -> return ctx { frames = f { pc = pc f + 1 } : fs }
    []     -> return ctx

headStack :: Context -> Value
headStack = head . stack . head . frames

modifyStack :: Context -> ([Value] -> [Value]) -> Context
modifyStack ctx m = let f:fs = frames ctx -- shouldn't trigger
                    in ctx { frames = f { stack = m (stack f) } : fs }

step :: Instruction -> Context -> IO Context
step i ctx = -- trace (show $ stack ctx) $
  case i of

    Call fname        -> if fname `M.notMember` fns ctx
                         then error $ "Function '" ++ fname ++ "' not found"
                         else case fns ctx M.! fname of
                                Defined args@(Limited argc) body -> let f:fs = frames ctx
                                                                    in withArgs fname args (stack f) $
                                                                       \arg ->
                                                                         return ctx { frames = Frame body 0 arg
                                                                                               : f { stack = drop argc $ stack f }
                                                                                               : fs }
                                Defined args@(Ellipses argc) body -> let f:fs = frames ctx
                                                                     in withArgs fname args (stack f) $
                                                                        \arg ->
                                                                          return ctx { frames = Frame body 0 arg
                                                                                       : f { stack = [] }
                                                                                       : fs }
                                Internal args fn -> let stk = stack $ head $ frames ctx
                                                    in withArgs fname args stk $ \_ -> fn stk >>=
                                                                                       \c -> advance $
                                                                                             modifyStack ctx (const c)
                                Mixed args fn -> let stk = stack $ head $ frames ctx
                                                 in withArgs fname args stk $ \_ -> fn ctx run stk >>=
                                                                                    \c -> advance $ modifyStack ctx (const c)

                                External fn argc rets -> let stk = stack $ head $ frames ctx
                                                         in withArgs fname (Limited argc) stk
                                                            $ \vals -> do
                                                                val <- callWith fn vals rets
                                                                advance $ modifyStack ctx $ \stk -> val:drop argc stk

    PushInt n     -> advance $ modifyStack ctx $ \stk -> ValInt n : stk

    PushFloat n   -> advance $ modifyStack ctx $ \stk -> ValFloat n : stk

    PushStr s     -> advance $ modifyStack ctx $ \stk -> ValStack (map ValChar s) : stk

    PushChar c    -> advance $ modifyStack ctx $ \stk -> ValChar c : stk

    PushFn f -> advance $ modifyStack ctx $ \stk -> ValFn (fns ctx M.! f) : stk

    PushFnVal args is -> advance $ modifyStack ctx $ \stk -> ValFn (Defined args $ V.fromList is) : stk

    Label _       -> advance ctx

    Goto l        -> performGo l ctx

    JumpTrue l    -> let ctx' = modifyStack ctx $ \(top:rs) -> rs -- TODO: Handle empty stack
                         top = headStack ctx
                     in if truthy top
                        then performGo l ctx'
                        else advance ctx'

    ForkTo l -> do
      side <- async $ do
        lbl <- performGo l ctx
        run lbl { frames = [head (frames lbl)] }
      advance $ modifyStack ctx $ \stk -> ValAbstract (Abs $ toDyn side) : stk

    GotoPos _     -> error "Impossible instruction hit"
    PosMarker _ _ -> error "Impossible instruction hit"
                     -- ^ due to being eliminated in IR pass
    Exit          -> error "Impossible instruction hit"
                     -- ^ due to being handled in 'run'
  where
    performGo :: String -> Context -> IO Context
    performGo label ctx' = let f:fs = frames ctx'
                               n = fst $ fromJust $ find ((==Label label) . snd) $
                                   zip [0..] (V.toList $ prog f)
                          in return ctx' { frames = f { pc = n } : fs }

runProgram :: FuncTable -> IO Context
runProgram table = do
  case table M.!? "main" of
    Just (Defined (Limited 0) main) ->
      run $ Ctx {
      frames = [Frame { prog = main, pc = 0, stack = [] }],
      fns = coreTable `M.union` table }
    Just _ -> error "Expected main function to take no arguments"
    Nothing -> error "Expected a main function"
