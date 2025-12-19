{-# LANGUAGE CPP #-}
module FFI where

#ifdef mingw32_HOST_OS
#else
import FFI.Unix
#endif

import           Core           (Value (..))
import           Foreign.C      (CChar (CChar), CDouble (CDouble),
                                 castCharToCChar)
import           Foreign.LibFFI (Arg, argCChar, argCDouble, argCInt, callFFI, retVoid)
import Foreign (FunPtr)
import qualified Data.Map as M

val2Arg :: Value -> Arg
val2Arg v =
  case v of
    ValNum n ->
      if snd (properFraction n) == 0
      then argCInt $ floor n
      else argCDouble $ CDouble n
    ValChar c -> argCChar $ castCharToCChar c
    ValBool b -> argCChar $ fromIntegral $ fromEnum b
    ValStack _ -> error "TODO: Stack -> C FFI"
    ValFn _ -> error "TODO: Fn -> C FFI"
    ValAbstract _ -> error "TODO: Abstract -> C FFI"

callWith :: FunPtr () -> [Value] -> IO ()
callWith func args = do
  -- putStrLn $ "Calling " ++ show func ++ " with " ++ show args
  callFFI func retVoid $ map val2Arg args

type FFITable = M.Map String SharedLib

closeTable :: FFITable -> IO ()
closeTable = mapM_ (\(_, lib) -> closeShared lib) . M.toList
