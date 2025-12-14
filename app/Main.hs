module Main (main) where

import           Control.Monad      (liftM2)
import           Core               (Function (..))
import           Data.Functor       (void)
import qualified Data.Map           as M
import           Interpreter        (runProgram)
import           IRPasses
import           Parser             (TopLevel (..), parseProgram)
import           System.Environment (getArgs)
import           System.FilePath    (dropFileName, (</>))
import           Text.Parsec        (parse)
import           Traverser          (Grid (Grid), IREmitter (runEmitter),
                                     traverse)

-- TODO: Find a place for this logic:
data ProgContext = ProgCtx { root :: FilePath, namespace :: [String] }
                 deriving (Show)

prefixNS :: [String] -> String -> String
prefixNS stk name = foldl (\nm a -> a ++ "." ++ nm) name stk  -- Concats in reverse!!

makeProg :: ProgContext -> [TopLevel] -> IO (Maybe [BuildUnit])
makeProg _ [] = return $ Just []
makeProg ctx (UseDrv s ns:tls) = do
  res <- parse parseProgram "" <$> readFile (root ctx </> (s ++ ".ch"))
  case res of
    Left e -> print e >> return Nothing
    Right imp -> do
      rest <- makeProg ctx tls
      this <- makeProg ctx { namespace = maybe (namespace ctx) (:namespace ctx) ns } imp
      return $ liftM2 (++) rest this
makeProg ctx (FuncDecl (name, argc, body):tls) = do
  case runEmitter (Traverser.traverse (Grid body) (0,0)) [] of
    Left e -> print e >> return Nothing
    Right (_, instrs) -> do
      rest <- makeProg ctx tls
      let instrs' = foregoPos instrs
      case rest of
        Nothing -> return Nothing
        Just [] -> return $ Just [Unit (M.singleton name (prefixNS (namespace ctx) name)) $ M.singleton name (argc, instrs')]
        Just ((Unit syms tbl):progs) -> let syms' = M.insert name (prefixNS (namespace ctx) name) syms
                                            tbl' = M.insert name (argc, instrs') tbl
                                        in return $ Just $ Unit syms' tbl':progs

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
    then do
      putStrLn "Usage: charta <source-file.ch>"
      return ()
    else do
      let root' = dropFileName (head args)
      res <- parse parseProgram "" <$> readFile (head args)
      case res of
        Left e -> print e
        Right tls -> do
          res' <- makeProg ProgCtx { root = root', namespace = [] } tls
          case res' of
            Nothing  -> return ()
            Just progs -> void $ runProgram $ M.map (uncurry Defined) $
                          foldl M.union M.empty $ map canonicalizeNames progs
