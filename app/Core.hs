module Core where

import qualified Data.Map    as M
import Traverser (Instruction)
import Parser (num, Item (..), ItemValue (..))
import Text.Parsec (parse)
import GHC.Float (double2Int)

data Function = Defined [Instruction]
              | Internal ([Value] -> IO [Value])

instance Show Function where
  show (Defined instrs) = show instrs
  show (Internal _)     = "<internal fn>"

type FuncTable = M.Map String Function

data Value = ValStr String
           | ValNum Double
           | ValBool Bool
           deriving (Show, Eq)

truthy :: Value -> Bool
truthy (ValStr "") = False
truthy (ValStr _)  = True
truthy (ValNum 0)  = False
truthy (ValNum _)  = True
truthy (ValBool b) = b

numeric :: Value -> Double
numeric (ValNum n) = n
numeric (ValBool b) = if b then 1.0 else 0.0
numeric (ValStr s) = case parse num "" s of
                       Right Item { val = ItNum n } -> n
                       _ -> error "Failed conversion string->number"
-- Stack ops
dup :: [Value] -> IO [Value]
dup [] = return []
dup (v:vs) = return $ v:v:vs

pop :: [Value] -> IO [Value]
pop [] = return []
pop (v:vs) = return vs

swap :: [Value] -> IO [Value]
swap [] = return []
swap vs@[v] = return vs
swap (v1:v2:vs) = return $ v2:v1:vs

rot :: [Value] -> IO [Value]
rot [] = return []
rot vs@[v] = return vs
rot [v1, v2] = return [v2, v1]
rot (v1:v2:v3:vs) = return $ v3:v1:v2:vs

over :: [Value] -> IO [Value]
over [] = return []
over vs@[v] = return vs
over (v1:v2:vs) = return $ v2:v1:v2:vs

-- Arithmetic
add :: [Value] -> IO [Value]
add [] = return []
add vs@[v] = return vs
add (ValStr s1:ValStr s2:vs) = return $ ValStr (s1 ++ s2):vs
add (v1:v2:vs) = return $ ValNum (numeric v1 + numeric v2):vs

sub :: [Value] -> IO [Value]
sub [] = return []
sub vs@[v] = return vs
sub (v1:v2:vs) = return $ ValNum (numeric v1 - numeric v2):vs

mult :: [Value] -> IO [Value]
mult [] = return []
mult vs@[v] = return vs
mult (ValNum n:ValStr s:vs) = return $ ValStr (concat $ replicate (double2Int n) s):vs
mult (v1@(ValStr s):v2@(ValNum n):vs) = mult $ v2:v1:vs
mult (v1:v2:vs) = return $ ValNum (numeric v1 * numeric v2):vs

div' :: [Value] -> IO [Value]
div' [] = return []
div' vs@[v] = return vs
div' (v1:v2:vs) = return $ ValNum (numeric v1 / numeric v2):vs

-- Logic
pushTrue :: [Value] -> IO [Value]
pushTrue vs = return $ ValBool True:vs

pushFalse :: [Value] -> IO [Value]
pushFalse vs = return $ ValBool False:vs

equals :: [Value] -> IO [Value]
equals [] = return [ValBool False]
equals [v] = return [ValBool True]
equals (v1:v2:vs) = return $ ValBool (v1 == v2):vs

notEquals :: [Value] -> IO [Value]
notEquals [] = return [ValBool False]
notEquals [v] = return [ValBool True]
notEquals (v1:v2:vs) = return $ ValBool (v1 /= v2):vs

less :: [Value] -> IO [Value]
less [] = return [ValBool False]
less [v] = return [ValBool True]
less (v1:v2:vs) = return $ ValBool (numeric v1 < numeric v2):vs

greater :: [Value] -> IO [Value]
greater [] = return [ValBool False]
greater [v] = return [ValBool True]
greater (v1:v2:vs) = return $ ValBool (numeric v1 > numeric v2):vs

lessEq :: [Value] -> IO [Value]
lessEq [] = return [ValBool False]
lessEq [v] = return [ValBool True]
lessEq (v1:v2:vs) = return $ ValBool (numeric v1 <= numeric v2):vs

greaterEq :: [Value] -> IO [Value]
greaterEq [] = return [ValBool False]
greaterEq [v] = return [ValBool True]
greaterEq (v1:v2:vs) = return $ ValBool (numeric v1 >= numeric v2):vs

-- I/O
put :: [Value] -> IO [Value]
put [] = return []
put (ValStr s:vs) = putStrLn s >> return vs
put (ValBool True:vs) = putStrLn "⊤" >> return vs
put (ValBool False:vs) = putStrLn "⊥" >> return vs
put (ValNum n:vs) = print n >> return vs

coreTable :: FuncTable
coreTable = M.fromList $ map (\(name, fn) -> (name, Internal fn)) [
  ("⇈", dup), -- \upuparrows
  ("∅", pop), -- \emptyset
  ("↻", rot), -- \circlearrowright
  ("↕", swap), -- \updownarrow
  ("⤴", over), -- arrow pointing right then curving up
  ("+", add),
  ("-", sub),
  ("*", mult),
  ("/", div'),
  ("⊤", pushTrue), -- \top
  ("⊥", pushFalse), -- \bot
  ("=", equals),
  ("≠", notEquals), -- neq
  ("<", less),
  (">", greater),
  ("≤", lessEq), -- \leq
  ("≥", greaterEq), -- \geq
  ("put", put)
  ]
