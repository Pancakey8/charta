module FFI.Unix where
import System.Posix (DL, dlopen, RTLDFlags (RTLD_NOW), dlclose, dlsym)
import Foreign (FunPtr)

newtype SharedLib = SharedLib DL

loadShared :: String -> IO SharedLib
loadShared lib = SharedLib <$> dlopen lib [RTLD_NOW]

closeShared :: SharedLib -> IO ()
closeShared (SharedLib dl) = dlclose dl

loadSymbol :: SharedLib -> String -> IO (FunPtr a)
loadSymbol (SharedLib dl) = dlsym dl

