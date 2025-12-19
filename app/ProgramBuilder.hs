{-# LANGUAGE TupleSections #-}
module ProgramBuilder where
import           Control.Exception (IOException, try)
import           Core              (Function (..))
import           Data.Foldable     (forM_, for_)
import qualified Data.Map          as M
import           IRPasses          (foregoPos)
import           Parser            (TopLevel (FuncDecl, UseDrv, FFIDecl), parseProgram, Visibility (..))
import           StdLib            (stdTable)
import           System.Directory  (doesFileExist)
import           System.FilePath   (addExtension, dropFileName, hasExtension,
                                    takeFileName, (</>), takeDirectory)
import           Text.Parsec       (parse)
import qualified Traverser
import           Traverser         (EmitterError (..), Grid (Grid),
                                    Instruction (..), runEmitter)
import System.Environment (getExecutablePath)
import qualified Data.Vector as V

data SourceElem = Native (Visibility, Function)
                | Foreign (String, String, Int)
                deriving (Show)

data SourceTree = Source (M.Map String SourceElem) [SourceTree]
                | Namespace String SourceTree
                deriving (Show)

printSourceTree :: SourceTree -> IO ()
printSourceTree = go 4
  where
    indent i = replicate i ' '
    go i (Namespace ns t) = do
      putStrLn $ indent i ++ "- NS " ++ ns ++ ":"
      go (i+1) t
    go i (Source tbl ts) = do
      putStrLn $ indent i ++ "- Source:"
      putStrLn $ indent i ++ show tbl
      forM_ ts $ \t -> go (i+1) t

addChild :: SourceTree -> SourceTree -> SourceTree
addChild (Source m ts) t     = Source m (t:ts)
addChild (Namespace ns t) t' = Namespace ns $ addChild t t'

modifyChild :: SourceTree -> (SourceTree -> SourceTree) -> SourceTree
modifyChild src@(Source _ _) f = f src
modifyChild (Namespace _ t) f  = modifyChild t f

buildSource :: FilePath -> IO SourceTree
buildSource entry = buildSource' False entry $ dropFileName entry

sourceFile :: FilePath -> FilePath
sourceFile file = if hasExtension file then file else addExtension file ".ch"

buildSource' :: Bool -> FilePath -> FilePath -> IO SourceTree
buildSource' appendRoot file root = do
  let source = sourceFile file
      source' = if appendRoot then root </> source else source
  sourceExists <- doesFileExist source'
  if sourceExists
    then do
      res <- parse parseProgram "" <$> readFile source'
      case res of
        Left e -> error $ show e
        Right tls -> do
          buildFromTLs tls root
    else do
      exeDir <- takeDirectory <$> getExecutablePath
      let stdlib = exeDir </> "stdlib" </> source
      stdExists <- doesFileExist stdlib
      if stdExists
      then do
        res <- parse parseProgram "" <$> readFile stdlib
        case res of
          Left e -> error $ show e
          Right tls ->
            do
              (Source m ts)<- buildFromTLs tls root
              return $ Source (m `M.union` M.map (Native . (Visible,)) (stdTable M.! takeFileName file)) ts
      else error $ "Failed to find package '" ++ show file ++ "'"

buildFromTLs :: [TopLevel] -> FilePath -> IO SourceTree
buildFromTLs tls root = go tls M.empty []
  where
    go [] funcs imports =
      return $ Source funcs imports

    go (UseDrv pkg ns : rest) funcs imports = do
      pkgTree <- buildSource' True pkg root
      let wrapped =
            case ns of
              Nothing    -> pkgTree
              Just alias -> Namespace alias pkgTree
      go rest funcs (wrapped : imports)

    go (FuncDecl (name, args, body, vis) : rest) funcs imports =
      case runEmitter (Traverser.traverse (Grid body) (0,0)) [] of
        Left e -> do
          putStrLn $ "In '" ++ name ++ "', at position " ++ show (posn e)
          error $ what e
        Right (_, instrs) -> do
          let func = Defined args $ V.fromList instrs
          go rest (M.insert name (Native (vis, func)) funcs) imports

    go (FFIDecl d@(lib, name, args):rest) funcs imports = go rest (M.insert name (Foreign d) funcs) imports

flattenTree :: SourceTree -> M.Map String SourceElem
flattenTree tree = go tree ""
  where
    go (Source tbl ts) ns =
      let rest = map (`go` ns) ts
          tbl' = M.mapKeys (getName ns) $ M.mapWithKey (\k v ->
                                                          case v of
                                                            Native f -> Native $ rewrite ns k f
                                                            Foreign _ -> v
                                                       ) tbl
      in foldl M.union tbl' rest
      where
        getName ns name = if name `M.member` tbl
                          then
                            case tbl M.! name of
                              Native (Hidden, _) -> ns ++ name ++ "(hidden)"
                              _ -> ns ++ name
                          else name

        rewrite ns fname (v, Defined args instrs) = (v, Defined args $ V.map (rewriteTerm ns) instrs)
        rewrite _ _ (v, f) = (v, f)

        rewriteTerm ns (Call x) = Call $ getName ns x
        rewriteTerm ns (PushFn x) = PushFn $ getName ns x
        rewriteTerm ns (PushFnVal args is) = PushFnVal args $ map (rewriteTerm ns) is
        rewriteTerm _ x = x
    go (Namespace ns' t) ns = go t (ns ++ ns' ++ ".")
