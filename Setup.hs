import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Types.UnqualComponentName
import Distribution.Utils.Path (getSymbolicPath)
import System.Directory
import System.FilePath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { postBuild = postBuildCopyStdlib
  }

postBuildCopyStdlib
  :: Args
  -> BuildFlags
  -> PackageDescription
  -> LocalBuildInfo
  -> IO ()
postBuildCopyStdlib _ _ pkg lbi = do
  let exe : _ = executables pkg
      exeComponent = unUnqualComponentName (exeName exe)

      -- NOTE: buildDir is a field, not a function
      exeBuildDir =
        getSymbolicPath (buildDir lbi) </> exeComponent

      target = exeBuildDir </> "stdlib"

  createDirectoryIfMissing True target
  copyDirectoryRecursive "stdlib" target

copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  mapM_ (copyEntry src dst) entries

copyEntry :: FilePath -> FilePath -> FilePath -> IO ()
copyEntry src dst name = do
  let from = src </> name
      to   = dst </> name
  isDir <- doesDirectoryExist from
  if isDir
    then copyDirectoryRecursive from to
    else copyFile from to
