module Core where

import qualified Data.Map    as M
import Traverser (Instruction)
import Parser (num, Item (..), ItemValue (..))
import Text.Parsec (parse)
import GHC.Float (double2Int, int2Double)
import Data.Fixed (mod')
import Data.List (singleton, intercalate)

data Function = Defined Int [Instruction]
              | Internal ([Value] -> IO [Value])

instance Show Function where
  show (Defined argc instrs) = "(" ++ show argc ++ ")" ++ show instrs
  show (Internal _)     = "<internal fn>"

type FuncTable = M.Map String Function

data Value = ValStr String
           | ValNum Double
           | ValBool Bool
           | ValStack [Value]
           deriving (Show, Eq)

truthy :: Value -> Bool
truthy (ValStr s) = not $ null s
truthy (ValNum n) = n /= 0
truthy (ValBool b) = b
truthy (ValStack vs) = not $ null vs

numeric :: Value -> Double
numeric (ValNum n) = n
numeric (ValBool b) = if b then 1.0 else 0.0
numeric (ValStr s) = case parse num "" s of
                       Right Item { val = ItNum n } -> n
                       _ -> error "Failed conversion string->number"
numeric (ValStack _) = error "Failed conversion stack->number"

stringified :: Value -> String
stringified (ValStr s) = s
stringified (ValNum n) = show n
stringified (ValBool b) = if b then "⊤" else "⊥"
stringified (ValStack s) = "[" ++ intercalate ", " (map stringified s) ++ "]"

-- Stack ops
dup :: [Value] -> IO [Value]
dup [] = return []
dup (v:vs) = return $ v:v:vs

pop :: [Value] -> IO [Value]
pop [] = return []
pop (_:vs) = return vs

swap :: [Value] -> IO [Value]
swap [] = return []
swap vs@[_] = return vs
swap (v1:v2:vs) = return $ v2:v1:vs

rot :: [Value] -> IO [Value]
rot [] = return []
rot vs@[_] = return vs
rot [v1, v2] = return [v2, v1]
rot (v1:v2:v3:vs) = return $ v3:v1:v2:vs

over :: [Value] -> IO [Value]
over [] = return []
over vs@[_] = return vs
over (v1:v2:vs) = return $ v2:v1:v2:vs

pack :: [Value] -> IO [Value]
pack vs = return [ValStack vs]

splat :: [Value] -> IO [Value]
splat (ValStack stk:vs) = return $ stk ++ vs
splat vs = return vs

depth :: [Value] -> IO [Value]
depth vs = return $ ValNum (int2Double $ length vs):vs

empty :: [Value] -> IO [Value]
empty vs = return $ ValBool (null vs):vs

-- Arithmetic
add :: [Value] -> IO [Value]
add [] = return []
add vs@[_] = return vs
add (ValStr s1:ValStr s2:vs) = return $ ValStr (s1 ++ s2):vs
add (v1:v2:vs) = return $ ValNum (numeric v1 + numeric v2):vs

sub :: [Value] -> IO [Value]
sub [] = return []
sub vs@[_] = return vs
sub (v1:v2:vs) = return $ ValNum (numeric v1 - numeric v2):vs

mult :: [Value] -> IO [Value]
mult [] = return []
mult vs@[_] = return vs
mult (ValNum n:ValStr s:vs) = return $ ValStr (concat $ replicate (double2Int n) s):vs
mult (v1@(ValStr _):v2@(ValNum _):vs) = mult $ v2:v1:vs
mult (v1:v2:vs) = return $ ValNum (numeric v1 * numeric v2):vs

div' :: [Value] -> IO [Value]
div' [] = return []
div' vs@[_] = return vs
div' (v1:v2:vs) = return $ ValNum (numeric v1 / numeric v2):vs

modNum :: [Value] -> IO [Value]
modNum [] = return []
modNum vs@[_] = return vs
modNum (v1:v2:vs) = return $ ValNum (numeric v1 `mod'` numeric v2):vs

-- Logic
pushTrue :: [Value] -> IO [Value]
pushTrue vs = return $ ValBool True:vs

pushFalse :: [Value] -> IO [Value]
pushFalse vs = return $ ValBool False:vs

equals :: [Value] -> IO [Value]
equals [] = return [ValBool False]
equals [_] = return [ValBool True]
equals (v1:v2:vs) = return $ ValBool (v1 == v2):vs

notEquals :: [Value] -> IO [Value]
notEquals [] = return [ValBool False]
notEquals [_] = return [ValBool True]
notEquals (v1:v2:vs) = return $ ValBool (v1 /= v2):vs

less :: [Value] -> IO [Value]
less [] = return [ValBool False]
less [_] = return [ValBool True]
less (v1:v2:vs) = return $ ValBool (numeric v1 < numeric v2):vs

greater :: [Value] -> IO [Value]
greater [] = return [ValBool False]
greater [_] = return [ValBool True]
greater (v1:v2:vs) = return $ ValBool (numeric v1 > numeric v2):vs

lessEq :: [Value] -> IO [Value]
lessEq [] = return [ValBool False]
lessEq [_] = return [ValBool True]
lessEq (v1:v2:vs) = return $ ValBool (numeric v1 <= numeric v2):vs

greaterEq :: [Value] -> IO [Value]
greaterEq [] = return [ValBool False]
greaterEq [_] = return [ValBool True]
greaterEq (v1:v2:vs) = return $ ValBool (numeric v1 >= numeric v2):vs

boolAnd :: [Value] -> IO [Value]
boolAnd [] = return []
boolAnd vs@[_] = return vs
boolAnd (v1:v2:vs) = return $ ValBool (truthy v1 && truthy v2):vs

boolOr :: [Value] -> IO [Value]
boolOr [] = return []
boolOr vs@[_] = return vs
boolOr (v1:v2:vs) = return $ ValBool (truthy v1 || truthy v2):vs

boolNot :: [Value] -> IO [Value]
boolNot [] = return []
boolNot (v:vs) = return $ ValBool (not $ truthy v):vs

-- Conversions
asStr :: [Value] -> IO [Value]
asStr [] = return []
asStr (v:vs) = return $ ValStr (stringified v):vs

asNum :: [Value] -> IO [Value]
asNum [] = return []
asNum (v:vs) = return $ ValNum (numeric v):vs

asBool :: [Value] -> IO [Value]
asBool [] = return []
asBool (v:vs) = return $ ValBool (truthy v):vs

isStr :: [Value] -> IO [Value]
isStr vs@(ValStr _ :_) = return $ ValBool True:vs
isStr vs = return $ ValBool False:vs

isNum :: [Value] -> IO [Value]
isNum vs@(ValNum _ :_) = return $ ValBool True:vs
isNum vs = return $ ValBool False:vs

isBool :: [Value] -> IO [Value]
isBool vs@(ValBool _ :_) = return $ ValBool True:vs
isBool vs = return $ ValBool False:vs

isStack :: [Value] -> IO [Value]
isStack vs@(ValStack _ :_) = return $ ValBool True:vs
isStack vs = return $ ValBool False:vs

-- I/O
put :: [Value] -> IO [Value]
put [] = return []
put (ValStr s:vs) = putStrLn s >> return vs
put (ValBool True:vs) = putStrLn "⊤" >> return vs
put (ValBool False:vs) = putStrLn "⊥" >> return vs
put (ValNum n:vs) = print n >> return vs
put (ValStack s:vs) = mapM_ (put . singleton) s >> return vs

debug :: [Value] -> IO [Value]
debug vs = print vs >> return vs

coreTable :: FuncTable
coreTable = M.fromList $ concatMap (\(names, fn) -> [ (name, Internal fn) | name <- names ]) [
  (["⇈", "dup"], dup), -- \upuparrows
  (["∅", "pop"], pop), -- \emptyset
  (["↻", "rot"], rot), -- \circlearrowright
  (["↕", "swp"], swap), -- \updownarrow
  (["⤴", "ovr"], over), -- arrow pointing right then curving up
  (["▭", "pack"], pack), -- \rect
  (["⋮", "spt"], splat), -- \vdots
  (["≡", "dpt"], depth), -- \equiv
  (["·", "null"], empty), -- \cdot
  (["+"], add),
  (["-"], sub),
  (["*"], mult),
  (["/"], div'),
  (["%"], modNum),
  (["⊤", "T"], pushTrue), -- \top
  (["⊥", "F"], pushFalse), -- \bot
  (["="], equals),
  (["≠", "!="], notEquals), -- \neq
  (["<"], less),
  ([">"], greater),
  (["≤", "<="], lessEq), -- \leq
  (["≥", ">="], greaterEq), -- \geq
  (["∧", "&&"], boolAnd), -- \wedge
  (["∨", "||"], boolOr), -- \vee
  (["¬", "!"], boolNot), -- \neg
  (["str"], asStr),
  (["num"], asNum),
  (["bool"], asBool),
  (["¿str", "isStr"], isStr),
  (["¿num", "isNum"], isNum),
  (["¿bool", "isBool"], isBool),
  (["¿stk", "isStk"], isStack),
  (["put"], put),
  (["⚠", "dbg"], debug) -- \warning
  ]
