module Main (main) where

import           Control.Monad      (liftM2, when)
import           Core               (Function (..))
import           Data.Functor       (void)
import qualified Data.Map           as M
import           Interpreter        (runProgram)
import           IRPasses
import           Parser             (TopLevel (..), parseProgram)
import           System.Environment (getArgs)
import           System.FilePath    (dropFileName, (</>))
import System.Directory
import           Text.Parsec        (parse)
import           Traverser          (Grid (Grid), IREmitter (runEmitter),
                                     traverse, EmitterError (..))
import Control.Exception (try, IOException)
import Debug.Trace (trace)
import Data.Foldable (forM_)
import Paths_charta
import StdLib (stdTable)

-- TODO: Find a place for this logic:
data ProgContext = ProgCtx { root :: FilePath, namespace :: [String] }
                 deriving (Show)

prefixNS :: [String] -> String -> String
prefixNS stk name = foldl (\nm a -> a ++ "." ++ nm) name stk  -- Concats in reverse!!

makeProg :: ProgContext -> [TopLevel] -> IO (Maybe [BuildUnit])
makeProg _ [] = return $ Just []
makeProg ctx (UseDrv s ns:tls) = do
  let local = root ctx </> (s ++ ".ch")
  stdlib <- try (getDataFileName $ "stdlib" </> (s ++ ".ch")) :: IO (Either IOException FilePath) 
  let handleFile fp table = do
        res <- parse parseProgram "" <$> readFile fp
        case res of
          Left e -> print e >> return Nothing
          Right imp -> do
            rest <- makeProg ctx tls
            let ctx' = ctx { namespace = maybe (namespace ctx) (:namespace ctx) ns }
            this <- makeProg ctx' imp
            case table of
              Nothing -> return $ liftM2 (++) rest this
              Just tbl ->
                case this of
                  Just ((Unit syms tb):progs) ->
                    let syms' = syms `M.union` M.mapWithKey (\k _ -> prefixNS (namespace ctx') k) tbl
                        tb' = tb `M.union` tbl
                    in return $ liftM2 (++) rest $ Just $ Unit syms' tb':progs
                  _ -> return Nothing
  i <- doesPathExist local
  if i
    then handleFile local Nothing
    else case stdlib of
           Left _ -> error $ "Failed to find package '" ++ s ++ "'"
           Right fp -> handleFile fp $ Just (stdTable M.! s)
makeProg ctx (FuncDecl (name, argc, body):tls) = do
  case runEmitter (Traverser.traverse (Grid body) (0,0)) [] of
    Left e -> do
                putStrLn $ "In '" ++ name ++ "', at position " ++ show (posn e)
                putStrLn $ what e
                return Nothing
    Right (_, instrs) -> do
      rest <- makeProg ctx tls
      let instrs' = foregoPos instrs
      case rest of
        Nothing -> return Nothing
        Just [] -> return $ Just [Unit (M.singleton name (prefixNS (namespace ctx) name)) $
                                   M.singleton name $ Defined argc instrs']
        Just ((Unit syms tbl):progs) -> let syms' = M.insert name (prefixNS (namespace ctx) name) syms
                                            tbl' = M.insert name (Defined argc instrs') tbl
                                        in return $ Just $ Unit syms' tbl':progs

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      putStrLn "Usage: charta <source-file.ch> [opts]"
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
            Just progs -> do
                            when ("-ir" `elem` tail args) $ mapM_ display (M.toList prog)
                            void $ runProgram prog
                          where
                            prog = foldl M.union M.empty $ map canonicalizeNames progs
                            display (name, Defined args instrs) = do
                              putStrLn $ "fn " ++ name ++ "(" ++ show args ++ "):"
                              forM_ instrs $ \instr -> do
                                putStrLn $ "  " ++ show instr
                            display (name, Internal args _) = do
                              putStrLn $ "fn " ++ name ++ "(" ++ show args ++ "):"
                              putStrLn "  <internal>"
                            display (name, Mixed args _) = do
                              putStrLn $ "fn " ++ name ++ "(" ++ show args ++ "):"
                              putStrLn "  <internal>"
