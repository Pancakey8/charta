module Main (main) where

import           Control.Monad      (liftM2)
import           Core               (FuncTable, Function (..))
import           Data.Functor       (void)
import qualified Data.Map           as M
import           Interpreter        (runProgram)
import           IRPasses           (doPasses)
import           Parser             (TopLevel (..), parseProgram)
import           System.Environment (getArgs)
import           Text.Parsec        (parse)
import           Traverser          (Grid (Grid), IREmitter (runEmitter),
                                     traverse, Instruction (Call))
import System.FilePath ((</>), dropFileName)
import Debug.Trace (trace)

data ProgContext = ProgCtx { root :: FilePath, namespace :: [String] }
                 deriving (Show)

data Prog = Prog (M.Map String String) (M.Map String (Int, [Instruction]))
          deriving (Show)

prefixNS :: [String] -> String -> String
prefixNS stk name = foldl (\nm a -> a ++ "." ++ nm) name stk  -- Concats in reverse!!

makeProg :: ProgContext -> [TopLevel] -> IO (Maybe [Prog])
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
      let instrs' = doPasses instrs
      putStrLn $ "fn " ++ name ++ " (" ++ show argc ++ "):"
      mapM_ print instrs'
      putStrLn "==="
      case rest of
        Nothing -> return Nothing
        Just [] -> return $ Just [Prog (M.singleton name (prefixNS (namespace ctx) name)) $ M.singleton name (argc, instrs')]
        Just ((Prog syms tbl):progs) -> let syms' = M.insert name (prefixNS (namespace ctx) name) syms
                                            tbl' = M.insert name (argc, instrs') tbl
                                        in return $ Just $ Prog syms' tbl':progs

unite :: [Prog] -> M.Map String (Int, [Instruction])
unite [] = M.empty
unite (Prog names tbl:ps) = M.foldrWithKey resolve tbl names `M.union` unite ps
  where
    resolve local canon tbl =
      let tbl' = M.insert canon (tbl M.! local) $ M.delete local tbl
      in M.map (resolveBody local canon) tbl'
    resolveBody _ _ (argc, []) = (argc, [])
    resolveBody local canon (argc, Call x:is)
      | x == local = let (_, is') = resolveBody local canon (argc, is)
                     in (argc, Call canon : is')
      | otherwise = let (_, is') = resolveBody local canon (argc, is)
                     in (argc, Call x : is')
    resolveBody local canon (argc, i:is) = let (_, is') = resolveBody local canon (argc, is)
                                             in (argc, i : is')

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
          res <- makeProg ProgCtx { root = root', namespace = [] } tls
          case res of
            Nothing  -> return ()
            Just prog -> void $ runProgram $ M.map (uncurry Defined) $ unite prog
