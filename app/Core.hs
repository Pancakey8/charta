{-# LANGUAGE LambdaCase #-}
module Core where

import           Data.Char   (ord)
import           Data.Fixed  (mod')
import           Data.List   (intercalate)
import qualified Data.Map    as M
import           Data.Maybe  (isJust)
import           GHC.Float   (double2Int, int2Double)
import           Parser      (Item (..), ItemValue (..), num, Arguments (..))
import           Text.Parsec (parse)
import           Traverser   (Instruction)

data Function = Defined Arguments [Instruction]
              | Internal ([Value] -> IO [Value])
              | Mixed (Context -> Runner -> [Value] -> IO [Value])

instance Eq Function where
  _ == _ = False

instance Show Function where
  show (Defined _ _) = "<user-defined fn>"
  show (Internal _)          = "<internal fn>"
  show (Mixed _)             = "<internal fn>"

type FuncTable = M.Map String Function

data Frame = Frame { prog :: [Instruction], pc :: Int, stack :: [Value] }
           deriving (Show)

data Context = Ctx { frames :: [Frame], fns :: FuncTable }
             deriving (Show)

data Value = ValNum Double
           | ValBool Bool
           | ValChar Char
           | ValStack [Value]
           | ValFn Function
           deriving (Show, Eq)

truthy :: Value -> Bool
truthy (ValNum n)    = n /= 0
truthy (ValBool b)   = b
truthy (ValStack vs) = not $ null vs
truthy (ValChar ch)  = ch /= '\0'
truthy (ValFn _)     = True

maybeString :: [Value] -> Maybe String
maybeString = mapM (\case
                       ValChar c -> Just c
                       _ -> Nothing)

numeric :: Value -> Double
numeric (ValNum n) = n
numeric (ValBool b) = if b then 1.0 else 0.0
numeric (ValChar c) = int2Double $ ord c
numeric (ValStack s) =
  case maybeString s of
    Just str -> case parse num "" str of
                  Right Item { val = ItNum n } -> n
                  _ -> error "Failed conversion stack->number"
    Nothing -> error "Failed conversion stack->number"
numeric (ValFn _) = error "Failed conversion fn->number"

stringified :: Value -> String
stringified (ValChar c)  = [c]
stringified (ValNum n)   = show n
stringified (ValBool b)  = if b then "⊤" else "⊥"
stringified (ValStack s) =
  case maybeString s of
    Just str -> str
    Nothing  -> "[" ++ intercalate ", " (map stringified s) ++ "]"
stringified (ValFn _)    = "<fn>"

-- Stack ops
dup :: [Value] -> IO [Value]
dup []     = return []
dup (v:vs) = return $ v:v:vs

drp :: [Value] -> IO [Value]
drp []     = return []
drp (_:vs) = return vs

pop :: [Value] -> IO [Value]
pop (ValStack (v:vs):vs') = return $ v:ValStack vs:vs'
pop _                     = error "Fail: pop"

hop :: [Value] -> IO [Value]
hop (ValStack (v1:v2:vs):vs') = return $ v2:ValStack (v1:vs):vs'
hop _                         = error "Fail: hop"

firstStk :: [Value] -> IO [Value]
firstStk (ValStack vs@(v:_):vs') = return $ v:ValStack vs:vs'
firstStk _                       = error "Fail: fst"

secondStk :: [Value] -> IO [Value]
secondStk (ValStack vs@(_:v:_):vs') = return $ v:ValStack vs:vs'
secondStk _                         = error "Fail: snd"

bot :: [Value] -> IO [Value]
bot (ValStack vs:vs') = return $ last vs:ValStack (init vs):vs'
bot _                 = error "Fail: bot"

lastStk :: [Value] -> IO [Value]
lastStk (ValStack vs:vs') = return $ last vs:ValStack vs:vs'
lastStk _                 = error "Fail: lst"

swap :: [Value] -> IO [Value]
swap []         = return []
swap vs@[_]     = return vs
swap (v1:v2:vs) = return $ v2:v1:vs

rot :: [Value] -> IO [Value]
rot []            = return []
rot vs@[_]        = return vs
rot [v1, v2]      = return [v2, v1]
rot (v1:v2:v3:vs) = return $ v3:v1:v2:vs

rotRev :: [Value] -> IO [Value]
rotRev []            = return []
rotRev vs@[_]        = return vs
rotRev [v1, v2]      = return [v2, v1]
rotRev (v1:v2:v3:vs) = return $ v2:v3:v1:vs

over :: [Value] -> IO [Value]
over []         = return []
over vs@[_]     = return vs
over (v1:v2:vs) = return $ v2:v1:v2:vs

pack :: [Value] -> IO [Value]
pack vs = return [ValStack vs]

splat :: [Value] -> IO [Value]
splat (ValStack stk:vs) = return $ stk ++ vs
splat vs                = return vs

depth :: [Value] -> IO [Value]
depth vs = return $ ValNum (int2Double $ length vs):vs

empty :: [Value] -> IO [Value]
empty vs = return $ ValBool (null vs):vs

rev :: [Value] -> IO [Value]
rev vs = return $ reverse vs

shove :: [Value] -> IO [Value]
shove (v:vs) = return $ vs ++ [v]
shove []     = return []

bring :: [Value] -> IO [Value]
bring [] = return []
bring vs = return $ last vs : init vs

-- Arithmetic
add :: [Value] -> IO [Value]
add []                           = return []
add vs@[_]                       = return vs
add (ValStack s2:ValStack s1:vs) = return $ ValStack (s1 ++ s2):vs
add (v2:v1:vs)                   = return $ ValNum (numeric v1 + numeric v2):vs

sub :: [Value] -> IO [Value]
sub []         = return []
sub vs@[_]     = return vs
sub (v2:v1:vs) = return $ ValNum (numeric v1 - numeric v2):vs

mult :: [Value] -> IO [Value]
mult [] = return []
mult vs@[_] = return vs
mult (ValNum n:ValStack s:vs) = return $ ValStack (concat $ replicate (double2Int n) s):vs
mult (v1@(ValStack _):v2@(ValNum _):vs) = mult $ v2:v1:vs
mult (v2:v1:vs) = return $ ValNum (numeric v1 * numeric v2):vs

div' :: [Value] -> IO [Value]
div' []         = return []
div' vs@[_]     = return vs
div' (v2:v1:vs) = return $ ValNum (numeric v1 / numeric v2):vs

modNum :: [Value] -> IO [Value]
modNum []         = return []
modNum vs@[_]     = return vs
modNum (v2:v1:vs) = return $ ValNum (numeric v1 `mod'` numeric v2):vs

-- Logic
pushTrue :: [Value] -> IO [Value]
pushTrue vs = return $ ValBool True:vs

pushFalse :: [Value] -> IO [Value]
pushFalse vs = return $ ValBool False:vs

equals :: [Value] -> IO [Value]
equals []         = return [ValBool False]
equals [_]        = return [ValBool True]
equals (v2:v1:vs) = return $ ValBool (v1 == v2):vs

notEquals :: [Value] -> IO [Value]
notEquals []         = return [ValBool False]
notEquals [_]        = return [ValBool True]
notEquals (v2:v1:vs) = return $ ValBool (v1 /= v2):vs

less :: [Value] -> IO [Value]
less []         = return [ValBool False]
less [_]        = return [ValBool True]
less (v2:v1:vs) = return $ ValBool (numeric v1 < numeric v2):vs

greater :: [Value] -> IO [Value]
greater []         = return [ValBool False]
greater [_]        = return [ValBool True]
greater (v2:v1:vs) = return $ ValBool (numeric v1 > numeric v2):vs

lessEq :: [Value] -> IO [Value]
lessEq []         = return [ValBool False]
lessEq [_]        = return [ValBool True]
lessEq (v2:v1:vs) = return $ ValBool (numeric v1 <= numeric v2):vs

greaterEq :: [Value] -> IO [Value]
greaterEq []         = return [ValBool False]
greaterEq [_]        = return [ValBool True]
greaterEq (v2:v1:vs) = return $ ValBool (numeric v1 >= numeric v2):vs

boolAnd :: [Value] -> IO [Value]
boolAnd []         = return []
boolAnd vs@[_]     = return vs
boolAnd (v2:v1:vs) = return $ ValBool (truthy v1 && truthy v2):vs

boolOr :: [Value] -> IO [Value]
boolOr []         = return []
boolOr vs@[_]     = return vs
boolOr (v2:v1:vs) = return $ ValBool (truthy v1 || truthy v2):vs

boolNot :: [Value] -> IO [Value]
boolNot []     = return []
boolNot (v:vs) = return $ ValBool (not $ truthy v):vs

-- Conversions
asStr :: [Value] -> IO [Value]
asStr []     = return []
asStr (v:vs) = return $ ValStack (map ValChar $ stringified v):vs

asNum :: [Value] -> IO [Value]
asNum []     = return []
asNum (v:vs) = return $ ValNum (numeric v):vs

asBool :: [Value] -> IO [Value]
asBool []     = return []
asBool (v:vs) = return $ ValBool (truthy v):vs

ordVal :: [Value] -> IO [Value]
ordVal (v@(ValChar _):vs) = return $ ValNum (numeric v):vs
ordVal _                  = error "Expected conversion char->int"

chrVal :: [Value] -> IO [Value]
chrVal (ValNum n:vs) = return $ ValChar (toEnum $ double2Int n):vs
chrVal _             = error "Expected conversion int->char"

packStr :: [Value] -> IO [Value]
packStr [] = return [ValStack []]
packStr vs = let (s,vs') = break (\case ValChar _ -> False
                                        _ -> True) vs
             in return $ ValStack s:vs'

isStr :: [Value] -> IO [Value]
isStr vs@(ValStack v :_) = return $ ValBool (isJust $ maybeString v):vs
isStr vs                 = return $ ValBool False:vs

isNum :: [Value] -> IO [Value]
isNum vs@(ValNum _ :_) = return $ ValBool True:vs
isNum vs               = return $ ValBool False:vs

isBool :: [Value] -> IO [Value]
isBool vs@(ValBool _ :_) = return $ ValBool True:vs
isBool vs                = return $ ValBool False:vs

isChar :: [Value] -> IO [Value]
isChar vs@(ValChar _ :_) = return $ ValBool True:vs
isChar vs                = return $ ValBool False:vs

isStack :: [Value] -> IO [Value]
isStack vs@(ValStack _ :_) = return $ ValBool True:vs
isStack vs                 = return $ ValBool False:vs

isFn :: [Value] -> IO [Value]
isFn vs@(ValFn _ :_) = return $ ValBool True:vs
isFn vs              = return $ ValBool False:vs

-- Function operations
type Runner = Context -> IO Context

apply :: Context -> Runner -> [Value] -> IO [Value]
apply _ _ (ValFn (Internal f):vs) = f vs
apply ctx runner (ValFn (Mixed f):vs) = f ctx runner vs
apply ctx runner (ValFn (Defined args body):vs) = do
  case args of
    Limited argc -> do
      ctx' <- runner ctx { frames = [ Frame { prog = body, pc = 0, stack = take argc vs } ] }
      return $ stack (head $ frames ctx') ++ drop argc vs
    Ellipses argc -> do
      ctx' <- runner ctx { frames = [ Frame { prog = body, pc = 0, stack = take argc vs ++ [ValStack (drop argc vs)] } ] }
      return $ stack (head $ frames ctx')
apply _ _ _ = error "Apply expected function"

applyLocal :: Context -> Runner -> [Value] -> IO [Value]
applyLocal _ _ (ValFn (Internal f):ValStack s:vs) = f s >>= \s' -> return $ ValStack s' : vs
applyLocal ctx runner (ValFn (Mixed f):ValStack s:vs) = f ctx runner s >>= \s' -> return $ ValStack s' : vs
applyLocal ctx runner (ValFn (Defined args body):ValStack s:vs) = do
  case args of
    Limited argc -> do
      ctx' <- runner ctx { frames = [ Frame { prog = body, pc = 0, stack = take argc s } ] }
      return $ ValStack (stack (head $ frames ctx') ++ drop argc s) : vs
    Ellipses argc -> do
      ctx' <- runner ctx { frames = [ Frame { prog = body, pc = 0, stack = take argc s ++ [ValStack (drop argc s)] } ] }
      return $ ValStack (stack (head $ frames ctx')) : vs
applyLocal _ _ _ = error "Apply local expected function & stack"

-- I/O
put :: [Value] -> IO [Value]
put []                  = return []
put (ValChar c:vs)      = putStrLn [c] >> return vs
put (ValBool True:vs)   = putStrLn "⊤" >> return vs
put (ValBool False:vs)  = putStrLn "⊥" >> return vs
put (ValNum n:vs)       = print n >> return vs
put (v@(ValFn _):vs)    = print (stringified v) >> return vs
put (v@(ValStack _):vs) = putStrLn (stringified v) >> return vs

debug :: [Value] -> IO [Value]
debug vs = mapM_ (putStrLn . stringified) vs >> return vs

coreTable :: FuncTable
coreTable = M.fromList $ concatMap (\(names, fn) -> [ (name, Internal fn) | name <- names ]) [
  (["⇈", "dup"], dup), -- \upuparrows
  (["∅", "drp"], drp), -- \emptyset
  (["⊢", "fst"], firstStk), -- \vdash
  (["⊩", "snd"], secondStk), -- \Vdash
  (["⊣", "lst"], lastStk), -- \dashv
  (["⊢!", "pop"], pop),
  (["⊩!", "hop"], hop),
  (["⊣!", "bot"], bot),
  (["↻", "rot"], rot), -- \circlearrowright
  (["↷", "rot-"], rotRev), -- \curvearrowright
  (["↕", "swp"], swap), -- \updownarrow
  (["⊼", "ovr"], over), -- \barwedge
  (["▭", "pack"], pack), -- \rect
  (["⋮", "spt"], splat), -- \vdots
  (["≡", "dpt"], depth), -- \equiv
  (["·", "null"], empty), -- \cdot
  (["⇆", "rev"], rev), -- \leftrightarrows
  (["⇓", "shv"], shove), -- \Downarrow
  (["⇑", "brg"], bring), -- \Uparrow
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
  (["ord"], ordVal),
  (["chr"], chrVal),
  (["▭s"], packStr),
  (["¿str", "isStr"], isStr),
  (["¿num", "isNum"], isNum),
  (["¿bool", "isBool"], isBool),
  (["¿char", "isChar"], isChar),
  (["¿stk", "isStk"], isStack),
  (["¿fn", "isFn"], isFn),
  (["put"], put),
  (["⚠", "dbg"], debug) -- \warning
  ] ++ concatMap (\(names, fn) -> [ (name, Mixed fn) | name <- names ]) [
  (["∘", "ap"], apply), -- \circ
  (["⊡", "sap"], applyLocal) -- \dotsquare
  ]
