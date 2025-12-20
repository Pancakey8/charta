{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Analyzer where
import           Control.Monad (foldM, foldM_)
import           Core          (FuncTable, Function (..), TValue, atLeast)
import           Data.Functor  (void)
import qualified Data.Map      as M
import qualified Data.Vector   as V
import qualified Parser        as P
import           Traverser     (Instruction (..))
import Debug.Trace (trace)

data Type = TInt
          | TFloat
          | TBool
          | TChar
          | TStack
          | TFn
          | TAbstract
          | TAny
          | TNumeric
          deriving (Show, Eq)

data Arguments = Limited [Type]
               | Ellipses [Type]
               deriving (Show, Eq)

data Returns = Exact [Type]
             | Many Type
             | Stop
             deriving (Show, Eq)

data FunctionAnalysis = Analysis Arguments Returns
                      deriving (Show)
type AnalysisTable = M.Map String FunctionAnalysis

coreAnalysis :: AnalysisTable
coreAnalysis = M.fromList $ concatMap (\(names, args, returns) -> [ (name, Analysis args returns) | name <- names ]) [
  (["⇈", "dup"], Limited [TAny], Exact [TAny, TAny]),
  (["∅", "drp"], Limited [TAny], Exact []),
  (["⊢", "fst"], Limited [TStack], Exact [TAny, TStack]),
  (["⊩", "snd"], Limited [TStack], Exact [TAny, TStack]),
  (["⊣", "lst"], Limited [TStack], Exact [TAny, TStack]),
  (["⊢!", "pop"], Limited [TStack], Exact [TAny, TStack]),
  (["⊩!", "hop"], Limited [TStack], Exact [TAny, TStack]),
  (["⊣!", "bot"], Limited [TStack], Exact [TAny, TStack]),
  (["↻", "rot"], Limited [TAny, TAny, TAny], Exact [TAny, TAny, TAny]),
  (["↷", "rot-"], Limited [TAny, TAny, TAny], Exact [TAny, TAny, TAny]),
  (["↕", "swp"], Limited [TAny, TAny], Exact [TAny, TAny]),
  (["⊼", "ovr"], Limited [TAny, TAny], Exact [TAny, TAny, TAny]),
  (["▭", "pack"], Ellipses [], Exact [TStack]),
  (["⋮", "spt"], Limited [TStack], Many TAny),
  (["≡", "dpt"], Ellipses [], Exact [TInt]),
  (["·", "null"], Ellipses [], Exact [TBool]),
  (["⇆", "rev"], Ellipses [], Many TAny),
  (["⇓", "shv"], Ellipses [TAny], Many TAny),
  (["⇑", "brg"], Ellipses [TAny], Many TAny),
  (["+"], Limited [TNumeric, TNumeric], Exact [TNumeric]),
  (["-"], Limited [TNumeric, TNumeric], Exact [TNumeric]),
  (["*"], Limited [TNumeric, TNumeric], Exact [TNumeric]),
  (["/"], Limited [TNumeric, TNumeric], Exact [TNumeric]),
  (["%"], Limited [TNumeric, TNumeric], Exact [TNumeric]),
  (["⊤", "T"], Limited [], Exact [TBool]),
  (["⊥", "F"], Limited [], Exact [TBool]),
  (["="], Limited [TAny, TAny], Exact [TBool]),
  (["≠", "!="], Limited [TAny, TAny], Exact [TBool]),
  (["<"], Limited [TNumeric, TNumeric], Exact [TBool]),
  ([">"], Limited [TNumeric, TNumeric], Exact [TBool]),
  (["≤", "<="], Limited [TNumeric, TNumeric], Exact [TBool]),
  (["≥", ">="], Limited [TNumeric, TNumeric], Exact [TBool]),
  (["∧", "&&"], Limited [TBool, TBool], Exact [TBool]),
  (["∨", "||"], Limited [TBool, TBool], Exact [TBool]),
  (["¬", "!"], Limited [TBool], Exact [TBool]),
  (["str"], Limited [TAny], Exact [TStack]),
  (["num"], Limited [TAny], Exact [TNumeric]),
  (["bool"], Limited [TAny], Exact [TBool]),
  (["ord"], Limited [TChar], Exact [TInt]),
  (["chr"], Limited [TInt], Exact [TChar]),
  (["▭s"], Ellipses [], Exact [TStack]),
  (["¿str", "is-str"], Limited [TAny], Exact [TBool]),
  (["¿num", "is-num"], Limited [TAny], Exact [TBool]),
  (["¿flt", "is-flt"], Limited [TAny], Exact [TBool]),
  (["¿int", "is-int"], Limited [TAny], Exact [TBool]),
  (["¿bool", "is-bool"], Limited [TAny], Exact [TBool]),
  (["¿char", "is-char"], Limited [TAny], Exact [TBool]),
  (["¿stk", "is-stk"], Limited [TAny], Exact [TBool]),
  (["¿fn", "is-fn"], Limited [TAny], Exact [TBool]),
  (["⚠", "dbg"], Limited [], Exact []),
  (["⊗", "pnc"], Ellipses [TAny], Stop),
  (["≻", "join"], Limited [TAbstract], Many TAny),
  (["∘", "ap"], Ellipses [TFn], Many TAny),
  (["⊡", "sap"], Ellipses [TFn, TStack], Exact [TStack])
  ]

warn :: String -> IO ()
warn msg = putStrLn $ "WARNING: " ++ msg

fault :: String -> IO a
fault msg = error $ "FAULT: " ++ msg

data Status = Unresolved String
            | FailArgs [Type] [Type]

emulate :: AnalysisTable -> [Instruction] -> [Type] -> Either Status Returns
emulate tbl [] stk = return $ Exact stk
emulate tbl (i:is) stk = trace ("At '" ++ show i ++ "', stack = " ++ show stk) $
  case i of
    Call f -> if f `M.notMember` tbl
              then Left $ Unresolved f
              else
                let (Analysis args rets) = tbl M.! f
                in case rets of
                     Many t -> return $ Many t
                     Exact n ->
                       takeArgs args stk $
                       \stk' -> emulate tbl is (n ++ stk')

    PushFloat _ -> emulate tbl is (TFloat:stk)
    PushInt _ -> emulate tbl is (TInt:stk)
    PushStr _ -> emulate tbl is (TStack:stk)
    PushChar _ -> emulate tbl is (TChar:stk)
    PushFn _ -> emulate tbl is (TFn:stk)
    PushFnVal _ _ -> emulate tbl is (TFn:stk)

    Label l -> error "TODO"
    Goto l -> error "TODO"

    JumpTrue l -> error "TODO"
    ForkTo l -> error "TODO"

    Exit -> return $ Exact stk

    PosMarker _ _ -> error "Unreachable PosMarker in Analysis"
    GotoPos _ -> error "Unreachable GotoPos in Analysis"

takeArgs :: Arguments -> [Type] -> ([Type] -> Either Status Returns) -> Either Status Returns
takeArgs args stk f = do
  case args of
    Limited n ->
      if isFitting n stk
      then
        f $ drop (length n) stk
      else
        Left $ FailArgs n stk
    Ellipses n ->
      if isFitting n stk
      then
        f []
      else
        Left $ FailArgs n stk
  where
    isFitting expecteds params =
      atLeast (length expecteds) params
      && all (uncurry matches) (zip expecteds params)
    matches expected got =
      case (expected, got) of
        (TInt, TInt)           -> True
        (TFloat, TFloat)       -> True
        (TBool, TBool)         -> True
        (TChar, TChar)         -> True
        (TStack, TStack)       -> True
        (TFn, TFn)             -> True
        (TAbstract, TAbstract) -> True
        (TNumeric, TNumeric)   -> True
        (TNumeric, TInt)       -> True
        (TNumeric, TFloat)     -> True
        (TAny, _)              -> True

analyzeFn :: AnalysisTable -> String -> Function -> IO AnalysisTable
analyzeFn tbl fname (Defined args instrs) = do
  case emulate tbl (V.toList instrs) [] of
    Left (Unresolved other) -> warn ("Unresolved '" ++ fname ++ "' due to '" ++ other ++ "'") >> return tbl
    Left (FailArgs expected got) -> fault ("Expected: " ++ show expected ++ "\nGot: " ++ show got)
    Right ret -> return $ M.insert fname (Analysis args' ret) tbl
  where
    args' = case args of
              P.Limited n  -> Limited $ replicate n TAny
              P.Ellipses n -> Ellipses $ replicate n TAny
analyzeFn tbl fname (External _ n rets) =
  let retType = case rets of
                  "int"    -> TInt
                  "double" -> TFloat
                  "char"   -> TChar
                  "bool"   -> TBool
                  _        -> error $ "Unsupported return type " ++ rets
  in return $ M.insert fname (Analysis (Limited (replicate n TAny)) (Exact [retType])) tbl
analyzeFn tbl fname _  = if fname `M.member` tbl
                         then return tbl
                         else warn ("'" ++ fname ++ "' has no internal type definition")
                              >> return tbl

analyzeProgram :: FuncTable -> IO ()
analyzeProgram table = do
  tbl <- foldM (\tbl (name, fn) -> analyzeFn tbl name fn) coreAnalysis $ M.toList table
  print tbl
