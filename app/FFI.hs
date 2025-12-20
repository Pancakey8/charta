{-# LANGUAGE CPP #-}
module FFI where



import           FFI.Unix


import           Core           (Value (..), valueTagOf)
import           Data.Coerce    (coerce)
import qualified Data.Map       as M
import           Foreign        (FunPtr, nullPtr, withArray)
import           Foreign.C      (CChar (CChar), CDouble (CDouble),
                                 castCCharToChar, castCharToCChar)
import           Foreign.LibFFI (Arg, RetType, argCChar, argCDouble, argCInt,
                                 argPtr, callFFI, retCChar, retCInt, retVoid)

val2Arg :: Value -> (Arg -> IO a) -> IO a
val2Arg v k =
  case v of
    ValNum n ->
      if snd (properFraction n) == 0
      then k $ argCInt $ floor n
      else k $ argCDouble $ CDouble n

    ValChar c ->
      k $ argCChar $ castCharToCChar c

    ValBool b ->
      k $ argCChar $ fromIntegral $ fromEnum b

    ValStack [] -> k $ argPtr nullPtr

    ValStack stk@(v:_) ->
      case v of
        ValChar _ ->
          withArray
          [ castCharToCChar c | ValChar c <- stk ]
          (k . argPtr)

        ValBool _ ->
          withArray
          [ fromIntegral (fromEnum b) | ValBool b <- stk ]
          (k . argPtr)

        ValNum _ ->
          let ns = [ n | ValNum n <- stk ]
          in if all (\t -> snd (properFraction t) == 0) ns
             then withArray
                  [ fromIntegral (floor n) | n <- ns ]
                  (k . argPtr)
             else withArray
                  [ CDouble n | n <- ns ]
                  (k . argPtr)

        _ ->
          error "Heterogeneous stack -> C FFI not supported"

    ValFn _ ->
      error "Fn -> C FFI not supported"

    ValAbstract _ ->
      error "Abstract -> C FFI not supported"

vals2Args :: [Value] -> ([Arg] -> IO a) -> IO a
vals2Args vals k =
  foldr step k vals []
  where
    step v cont acc =
      val2Arg v $ \arg ->
        cont (arg : acc)

callWith :: FunPtr () -> [Value] -> String -> IO Value
callWith func args rets =
  case rets of
    "int" -> do
      n <- callFFIReturn retCInt
      return $ ValNum $ fromIntegral n
    "char" -> do
      n <- callFFIReturn retCChar
      return $ ValChar $ castCCharToChar n
    "bool" -> do
      n <- callFFIReturn retCChar
      return $ ValBool $ toEnum $ fromIntegral n
  where
    callFFIReturn ret = vals2Args args $ \args' -> do
      callFFI func ret args'

type FFITable = M.Map String SharedLib

closeTable :: FFITable -> IO ()
closeTable = mapM_ (\(_, lib) -> closeShared lib) . M.toList
