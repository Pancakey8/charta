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
                                 argPtr, callFFI, retCChar, retCInt, retVoid, retCDouble)

val2Arg :: Value -> (Arg -> IO a) -> IO a
val2Arg v k =
  case v of
    ValInt n -> k $ argCInt $ fromIntegral n

    ValFloat n -> k $ argCDouble $ CDouble n

    ValChar c ->
      k $ argCChar $ castCharToCChar c

    ValBool b ->
      k $ argCChar $ fromIntegral $ fromEnum b

    ValStack [] -> k $ argPtr nullPtr

    ValStack stk@(v:_) ->
      let t = valueTagOf v
      in if all ((==t) . valueTagOf) stk
         then case v of
                ValChar _ ->
                  withArray
                  [ castCharToCChar c | ValChar c <- stk ]
                  (k . argPtr)

                ValBool _ ->
                  withArray
                  [ fromIntegral (fromEnum b) | ValBool b <- stk ]
                  (k . argPtr)

                ValInt _ ->
                  withArray
                  [ n | ValInt n <- stk ]
                  (k . argPtr)

                ValFloat _ ->
                  withArray
                  [ n | ValFloat n <- stk ]
                  (k . argPtr)

                ValFn _ -> error "Fn -> C FFI not supported"
                ValAbstract _ -> error "Abstract -> C FFI not supported"

         else error "Heterogeneous stack -> C FFI not supported"

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
      return $ ValInt $ fromIntegral n
    "double" -> do
      CDouble n <- callFFIReturn retCDouble
      return $ ValFloat n
    "char" -> do
      n <- callFFIReturn retCChar
      return $ ValChar $ castCCharToChar n
    "bool" -> do
      n <- callFFIReturn retCChar
      return $ ValBool $ toEnum $ fromIntegral n
    _ -> error $ "Unsupported return type " ++ rets
  where
    callFFIReturn ret = vals2Args args $ \args' -> do
      callFFI func ret args'

type FFITable = M.Map String SharedLib

closeTable :: FFITable -> IO ()
closeTable = mapM_ (\(_, lib) -> closeShared lib) . M.toList
