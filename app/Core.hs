{-# LANGUAGE LambdaCase #-}
module Core where

import           Control.Concurrent.Async (Async, wait)
import           Data.Char                (ord)
import           Data.Dynamic             (Dynamic, fromDynamic)
import           Data.Fixed               (mod')
import           Data.List                (intercalate)
import qualified Data.Map                 as M
import           Data.Maybe               (isJust)
import qualified Data.Vector              as V
import           Foreign                  (FunPtr)
import           GHC.Float                (double2Int, int2Double)
import           Parser                   (Arguments (..), Item (..),
                                           ItemValue (..), num)
import           System.Exit              (exitFailure)
import           Text.Parsec              (parse)
import           Traverser                (Instruction)

data Function = Defined Arguments (V.Vector Instruction)
              | Internal Arguments ([Value] -> IO [Value])
              | Mixed Arguments (Context -> Runner -> [Value] -> IO [Value])
              | External (FunPtr ()) Int

instance Eq Function where
  _ == _ = False

instance Show Function where
  show (Defined {})   = "<user-defined fn>"
  show (Internal _ _) = "<internal fn>"
  show (Mixed _ _)    = "<internal fn>"
  show (External _ _)      = "<foreign fn>"

type FuncTable = M.Map String Function

data Frame = Frame { prog :: V.Vector Instruction, pc :: Int, stack :: [Value] }
           deriving (Show)

data Context = Ctx { frames :: [Frame], fns :: FuncTable }
             deriving (Show)

newtype Abstract = Abs Dynamic

instance Eq Abstract where
  _ == _ = False

instance Show Abstract where
  show _ = "<abstract>"

data Value = ValNum {-# UNPACK #-} !Double
           | ValBool !Bool
           | ValChar !Char
           | ValStack [Value]
           | ValFn Function
           | ValAbstract Abstract
           deriving (Show, Eq)

truthy :: Value -> Bool
truthy (ValNum n)      = n /= 0
truthy (ValBool b)     = b
truthy (ValStack vs)   = not $ null vs
truthy (ValChar ch)    = ch /= '\0'
truthy (ValFn _)       = True
truthy (ValAbstract _) = True

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
numeric (ValAbstract _) = error "Failed conversion abstract->int"

stringified :: Value -> String
stringified (ValChar c)  = [c]
stringified (ValNum n)   = show n
stringified (ValBool b)  = if b then "⊤" else "⊥"
stringified (ValStack s) =
  case maybeString s of
    Just str -> str
    Nothing  -> "[" ++ intercalate ", " (map stringified s) ++ "]"
stringified (ValFn _)    = "<fn>"
stringified (ValAbstract _) = "<abstract>"

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

argCount :: Arguments -> Int
argCount (Limited n)  = n
argCount (Ellipses n) = n

atLeast :: Int -> [a] -> Bool
atLeast 0 _      = True
atLeast _ []     = False
atLeast k (_:xs) = atLeast (k-1) xs

withArgs :: String -> Arguments -> [Value] -> ([Value] -> IO a) -> IO a
withArgs fname (Limited n) vs f
  | not (atLeast n vs) =
      error $
        "'" ++ fname ++ "' expects " ++ show n ++ " arguments"
  | otherwise =
      f (take n vs)

withArgs fname (Ellipses n) vs f
  | not (atLeast n vs) =
      error $
        "'" ++ fname ++ "' expects at least " ++ show n ++ " arguments"
  | otherwise =
      f (take n vs ++ [ValStack (drop n vs)])

apply :: Context -> Runner -> [Value] -> IO [Value]
apply _ _ (ValFn func@(Internal args f):vs) = withArgs (show func) args vs $ \_ -> f vs
apply ctx runner (ValFn func@(Mixed args f):vs) = withArgs (show func) args vs $ \_ -> f ctx runner vs
apply ctx runner (ValFn func@(Defined args body):vs) =
  withArgs (show func) args vs $
  \arg -> do
    ctx' <- runner ctx { frames = [ Frame { prog = body, pc = 0, stack = arg } ] }
    case args of
      Limited argc -> return $ stack (head $ frames ctx') ++ drop argc vs
      Ellipses _   -> return $ stack (head $ frames ctx')
apply _ _ _ = error "Apply expected function"

applyLocal :: Context -> Runner -> [Value] -> IO [Value]
applyLocal _ _ (ValFn (Internal args f):ValStack s:vs) = f s >>= \s' -> return $ ValStack s' : vs
applyLocal ctx runner (ValFn (Mixed args f):ValStack s:vs) = f ctx runner s >>= \s' -> return $ ValStack s' : vs
applyLocal ctx runner (ValFn func@(Defined args body):ValStack s:vs) =
  withArgs (show func) args s $
  \arg -> do
    ctx' <- runner ctx { frames = [ Frame { prog = body, pc = 0, stack = arg } ] }
    case args of
      Limited argc -> return $ ValStack (stack (head $ frames ctx') ++ drop argc s) : vs
      Ellipses _ -> return $ ValStack (stack (head $ frames ctx')) : vs
applyLocal _ _ _ = error "Apply local expected function & stack"

-- I/O
stringified' :: Value -> String
stringified' (ValStack s) =
  case maybeString s of
    Just str -> "\"" ++ str ++ "\""
    Nothing  -> stringified (ValStack s)
stringified' v = stringified v

debug :: [Value] -> IO [Value]
debug vs = putStrLn (show (length vs) ++ ":") >>
           mapM_ (putStrLn . (" - " ++) . stringified') vs >> return vs

panic :: [Value] -> IO [Value]
panic []    = putStrLn "Panic triggered without a message" >> exitFailure
panic (v:_) = putStrLn ("Panic triggered: \n" ++ stringified v) >> exitFailure

joinAsync :: [Value] -> IO [Value]
joinAsync (ValAbstract (Abs a):vs) =
  case fromDynamic a :: Maybe (Async Context) of
    Just td -> do
      ctx <- wait td
      let stk = stack $ head $ frames ctx
      return $ stk ++ vs
    Nothing -> error "join: Expected async, found abstract"

coreTable :: FuncTable
coreTable = M.fromList $ concatMap (\(names, args, fn) -> [ (name, Internal args fn) | name <- names ]) [
  (["⇈", "dup"], Limited 1, dup), -- \upuparrows
  (["∅", "drp"], Limited 1, drp), -- \emptyset
  (["⊢", "fst"], Limited 1, firstStk), -- \vdash
  (["⊩", "snd"], Limited 1, secondStk), -- \Vdash
  (["⊣", "lst"], Limited 1, lastStk), -- \dashv
  (["⊢!", "pop"], Limited 1, pop),
  (["⊩!", "hop"], Limited 1, hop),
  (["⊣!", "bot"], Limited 1, bot),
  (["↻", "rot"], Limited 3, rot), -- \circlearrowright
  (["↷", "rot-"], Limited 3, rotRev), -- \curvearrowright
  (["↕", "swp"], Limited 2, swap), -- \updownarrow
  (["⊼", "ovr"], Limited 2, over), -- \barwedge
  (["▭", "pack"], Ellipses 0, pack), -- \rect
  (["⋮", "spt"], Limited 1, splat), -- \vdots
  (["≡", "dpt"], Ellipses 0, depth), -- \equiv
  (["·", "null"], Ellipses 0, empty), -- \cdot
  (["⇆", "rev"], Ellipses 0, rev), -- \leftrightarrows
  (["⇓", "shv"], Ellipses 0, shove), -- \Downarrow
  (["⇑", "brg"], Ellipses 0, bring), -- \Uparrow
  (["+"], Limited 2, add),
  (["-"], Limited 2, sub),
  (["*"], Limited 2, mult),
  (["/"], Limited 2, div'),
  (["%"], Limited 2, modNum),
  (["⊤", "T"], Limited 0, pushTrue), -- \top
  (["⊥", "F"], Limited 0, pushFalse), -- \bot
  (["="], Limited 2, equals),
  (["≠", "!="], Limited 2, notEquals), -- \neq
  (["<"], Limited 2, less),
  ([">"], Limited 2, greater),
  (["≤", "<="], Limited 2, lessEq), -- \leq
  (["≥", ">="], Limited 2, greaterEq), -- \geq
  (["∧", "&&"], Limited 2, boolAnd), -- \wedge
  (["∨", "||"], Limited 2, boolOr), -- \vee
  (["¬", "!"], Limited 1, boolNot), -- \neg
  (["str"], Limited 1, asStr),
  (["num"], Limited 1, asNum),
  (["bool"], Limited 1, asBool),
  (["ord"], Limited 1, ordVal),
  (["chr"], Limited 1, chrVal),
  (["▭s"], Ellipses 0, packStr),
  (["¿str", "is-str"], Limited 1, isStr),
  (["¿num", "is-num"], Limited 1, isNum),
  (["¿bool", "is-bool"], Limited 1, isBool),
  (["¿char", "is-char"], Limited 1, isChar),
  (["¿stk", "is-stk"], Limited 1, isStack),
  (["¿fn", "is-fn"], Limited 1, isFn),
  (["⚠", "dbg"], Ellipses 0, debug), -- \warning
  (["⊗", "pnc"], Ellipses 0, panic), -- \otimes
  (["≻", "join"], Limited 1, joinAsync)
  ] ++ concatMap (\(names, args, fn) -> [ (name, Mixed args fn) | name <- names ]) [
  (["∘", "ap"], Ellipses 1, apply), -- \circ
  (["⊡", "sap"], Ellipses 1, applyLocal) -- \dotsquare
  ]
