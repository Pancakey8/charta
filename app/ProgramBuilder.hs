module ProgramBuilder where
import           Control.Exception (IOException, try)
import           Core              (Function (..))
import           Data.Foldable     (forM_, for_)
import qualified Data.Map          as M
import           IRPasses          (foregoPos)
import           Parser            (TopLevel (FuncDecl, UseDrv), parseProgram)
import           Paths_charta
import           StdLib            (stdTable)
import           System.Directory  (doesFileExist)
import           System.FilePath   (addExtension, dropFileName, hasExtension,
                                    takeFileName, (</>))
import           Text.Parsec       (parse)
import qualified Traverser
import           Traverser         (EmitterError (..), Grid (Grid),
                                    Instruction (..), runEmitter)

data SourceTree = Source (M.Map String Function) [SourceTree]
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
buildSource entry = buildSource' entry $ dropFileName entry

sourceFile :: FilePath -> FilePath
sourceFile file = if hasExtension file then file else addExtension file ".ch"

buildSource' :: FilePath -> FilePath -> IO SourceTree
buildSource' file root = do
  let source = sourceFile file
  sourceExists <- doesFileExist source
  if sourceExists
    then do
      res <- parse parseProgram "" <$> readFile source
      case res of
        Left e -> error $ show e
        Right tls -> do
          buildFromTLs tls root
    else do
      stdlib <- try (getDataFileName ("stdlib" </> source)) :: IO (Either IOException FilePath)
      case stdlib of
        Left _ -> error $ "Failed to find package '" ++ show file ++ "'"
        Right fp -> do
          res <- parse parseProgram "" <$> readFile fp
          case res of
            Left e -> error $ show e
            Right tls -> do
              (Source m ts)<- buildFromTLs tls root
              return $ Source (m `M.union` (stdTable M.! takeFileName file)) ts

buildFromTLs :: [TopLevel] -> FilePath -> IO SourceTree
buildFromTLs tls root = go tls M.empty []
  where
    go [] funcs imports =
      return $ Source funcs imports

    go (UseDrv pkg ns : rest) funcs imports = do
      pkgTree <- buildSource' (root </> pkg) root
      let wrapped =
            case ns of
              Nothing    -> pkgTree
              Just alias -> Namespace alias pkgTree
      go rest funcs (wrapped : imports)

    go (FuncDecl (name, args, body) : rest) funcs imports =
      case runEmitter (Traverser.traverse (Grid body) (0,0)) [] of
        Left e -> do
          putStrLn $ "In '" ++ name ++ "', at position " ++ show (posn e)
          error $ what e
        Right (_, instrs) -> do
          let func = Defined args (foregoPos instrs)
          go rest (M.insert name func funcs) imports

flattenTree :: SourceTree -> M.Map String Function
flattenTree tree = go tree ""
  where
    go (Source tbl ts) ns =
      let rest = map (`go` ns) ts
          tbl' = M.mapKeys (ns ++) $ M.mapWithKey (rewrite ns) tbl
      in foldl M.union tbl' rest
      where
        rewrite ns fname (Defined args instrs) = Defined args $ map (rewriteTerm ns) instrs
        rewrite _ _ f = f
        rewriteTerm ns (Call x) = Call $ if x `M.member` tbl then ns ++ x else x
        rewriteTerm ns (PushFn x) = PushFn $ if x `M.member` tbl then ns ++ x else x
        rewriteTerm _ x = x
    go (Namespace ns' t) ns = go t (ns ++ ns' ++ ".")
