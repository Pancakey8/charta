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
                                     traverse)
import System.FilePath ((</>), dropFileName)
import Debug.Trace (trace)

newtype ProgContext = Prog { root :: FilePath }

makeProg :: ProgContext -> [TopLevel] -> IO (Maybe FuncTable)
makeProg _ [] = return $ Just M.empty
makeProg ctx (UseDrv s:tls) = do
  res <- parse parseProgram "" <$> readFile (root ctx </> (s ++ ".ch"))
  case res of
    Left e -> print e >> return Nothing
    Right imp -> do
      rest <- makeProg ctx tls
      this <- makeProg ctx imp
      return $ liftM2 M.union rest this
makeProg ctx (FuncDecl (name, argc, body):tls) = do
  case runEmitter (Traverser.traverse (Grid body) (0,0)) [] of
    Left e -> print e >> return Nothing
    Right (_, instrs) -> do
      rest <- makeProg ctx tls
      let instrs' = doPasses instrs
      return $ M.insert name (Defined argc instrs') <$> rest

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
          prog <- makeProg Prog { root = root' } tls
          case prog of
            Nothing  -> return ()
            Just tbl -> void $ runProgram tbl
