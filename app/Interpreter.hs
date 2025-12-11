module Interpreter where

import qualified Data.Map   as M

import           Data.List  (find)
import           Data.Maybe (fromJust)
import           Traverser  (Instruction (..))
import Debug.Trace (trace)
import Control.Exception (evaluate)

data Function = Defined [Instruction]
              | Internal ([Value] -> IO [Value])

instance Show Function where
  show (Defined instrs) = show instrs
  show (Internal _) = "<internal fn>"

type FuncTable = M.Map String Function

coreDup :: [Value] -> IO [Value]
coreDup [] = return []
coreDup (v:vs) = return $ v:v:vs

coreSwap :: [Value] -> IO [Value]
coreSwap [] = return []
coreSwap [a] = return [a]
coreSwap (a:b:vs) = return $ b:a:vs

corePut :: [Value] -> IO [Value]
corePut (ValStr s:vs) = putStrLn s >> return vs
corePut (ValNum n:vs) = print n >> return vs
corePut (ValBool b:vs) = print b >> return vs

coreLess :: [Value] -> IO [Value]
coreLess (ValNum a:ValNum b:vs) = return $ ValBool (a < b) : vs

coreSub :: [Value] -> IO [Value]
coreSub (ValNum a:ValNum b:vs) = return $ ValNum (a - b) : vs

core :: M.Map String Function
core = M.fromList $ map (\(name, fn) -> (name, Internal fn)) 
  [
    ("dup", coreDup),
    ("swap", coreSwap),
    ("put", corePut),
    ("less", coreLess),
    ("-", coreSub)
  ]

data Value = ValStr String
           | ValNum Double
           | ValBool Bool
           deriving (Show)

truthy :: Value -> Bool
truthy (ValStr "") = False
truthy (ValStr _)  = True
truthy (ValNum 0)  = False
truthy (ValNum _)  = True
truthy (ValBool b) = b

data Frame = Frame { prog :: [Instruction], pc :: Int }
           deriving (Show)

data Context = Ctx { frames :: [Frame], stack :: [Value], fns :: FuncTable }
             deriving (Show)

run :: Context -> IO Context
run ctx = -- trace (show ctx) $
  case frames ctx of
    [] -> return ctx
    f:fs ->
      let instr = prog f !! pc f
      in if instr == Exit
         then
           case fs of
             f':fs -> run ctx { frames = f' { pc = pc f' + 1 } : fs }
             [] -> run ctx { frames = [] }
         else step instr ctx >>= run

advance :: Context -> IO Context
advance ctx =
  case frames ctx of
    f : fs -> return ctx { frames = f { pc = pc f + 1 } : fs }
    []     -> return ctx

step :: Instruction -> Context -> IO Context
step i ctx = -- trace (show $ stack ctx) $
  case i of
    Call f        -> case fns ctx M.! f of
                       Defined body -> return ctx { frames = Frame body 0 : frames ctx }
                       Internal fn -> fn (stack ctx) >>= \c -> advance ctx { stack = c }

    PushNum n     -> advance ctx { stack = ValNum n : stack ctx }

    PushStr s     -> advance ctx { stack = ValStr s : stack ctx }

    Label l       -> advance ctx

    Goto l        -> performGo l ctx

    JumpTrue l    -> let (top:rs) = stack ctx
                     in if truthy top
                        then performGo l ctx { stack = rs }
                        else advance ctx { stack = rs }

    GotoPos _     -> error "Impossible instruction hit"
    PosMarker _ _ -> error "Impossible instruction hit"
                     -- ^ due to being eliminated in IR pass
    Exit          -> error "Impossible instruction hit"
                     -- ^ due to being handled in 'run'
  where
    performGo :: String -> Context -> IO Context
    performGo label ctx = let f:fs = frames ctx
                              n = fst $ fromJust $ find ((==Label label) . snd) $
                                  zip [0..] (prog f)
                          in return ctx { frames = f { pc = n } : fs }

runProgram :: FuncTable -> IO Context
runProgram table = do
  let Defined main = table M.! "main"
  run $ Ctx {
    frames = [Frame { prog = main, pc = 0 }],
    stack = [],
    fns = core `M.union` table }
