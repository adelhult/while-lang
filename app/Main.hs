{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative (Applicative (..))
import Control.Monad (when)
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (get, put), StateT, runStateT)
import Control.Monad.Writer (MonadWriter (tell), WriterT, execWriterT)
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Parse
  ( Expr (..),
    Number,
    Stmt (..),
    Variable,
    parseStmt,
  )
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)

-- Note: for practial purposes the state is represented as a map (instead
-- of a function Var -> Number like in the denotational semantics presented
-- in the course)
newtype State = State (Map Variable Number)

instance Show State where
  show (State state) =
    unlines $
      map (\(k, v) -> k ++ " ↦ " ++ show v) (Map.toList state)

emptyState :: State
emptyState = State Map.empty

newtype Interpreter a
  = Interpreter {runInterpreter :: WriterT String (StateT State Identity) a}
  deriving (Functor, Applicative, Monad, MonadState State, MonadWriter String)

getVar :: Variable -> Interpreter (Maybe Number)
getVar x = do
  State st <- get
  return $ Map.lookup x st

assignVar :: Variable -> Number -> Interpreter ()
assignVar x value = do
  State s <- get
  put $ State $ Map.insert x value s

-- | An extra pass to search for variables (helpful when init. a state)
findVariables :: Stmt -> Set Variable
findVariables stmt = case stmt of
  Assign x _ -> Set.fromList [x]
  Skip -> Set.empty
  Sequence ss -> foldl (\vars s -> vars `Set.union` findVariables s) Set.empty ss
  If cond yes no ->
    searchExpr cond
      `Set.union` findVariables yes
      `Set.union` findVariables no
  While cond loop ->
    searchExpr cond
      `Set.union` findVariables loop
  where
    searchExpr :: Expr a -> Set Variable
    searchExpr expr = case expr of
      -- contains no variables
      NumLit _ -> Set.empty
      TrueLit -> Set.empty
      FalseLit -> Set.empty
      -- we found a variable!
      Var x -> Set.fromList [x]
      -- recursivly look for variables
      Neg b -> searchExpr b
      Add lhs rhs -> binOp lhs rhs
      Mul lhs rhs -> binOp lhs rhs
      Sub lhs rhs -> binOp lhs rhs
      Eq lhs rhs -> binOp lhs rhs
      And lhs rhs -> binOp lhs rhs
      LessEq lhs rhs -> binOp lhs rhs
    binOp lhs rhs = searchExpr lhs `Set.union` searchExpr rhs

evalExpr :: Expr a -> Interpreter a
evalExpr expr = case expr of
  -- Arithmetic expressions
  NumLit n -> return n
  Var x -> do
    value <- getVar x
    case value of
      Just n -> return n
      -- Should be unreachable since we always init the state
      Nothing -> error $ "Variable '" ++ x ++ "' not found"
  Add lhs rhs -> liftA2 (+) (evalExpr lhs) (evalExpr rhs)
  Mul lhs rhs -> liftA2 (*) (evalExpr lhs) (evalExpr rhs)
  Sub lhs rhs -> liftA2 (-) (evalExpr lhs) (evalExpr rhs)
  -- Boolean expressions
  TrueLit -> return True
  FalseLit -> return False
  Eq lhs rhs -> liftA2 (==) (evalExpr lhs) (evalExpr rhs)
  LessEq lhs rhs -> liftA2 (<=) (evalExpr lhs) (evalExpr rhs)
  And lhs rhs -> liftA2 (&&) (evalExpr lhs) (evalExpr rhs)
  Neg b -> not <$> evalExpr b

evalStmt :: Stmt -> Interpreter ()
evalStmt stmt = case stmt of
  Assign x expr -> do
    tell ""
    value <- evalExpr expr
    assignVar x value
  Skip -> return ()
  Sequence ss -> mapM_ evalStmt ss
  If cond yes no -> do
    cond' <- evalExpr cond
    if cond'
      then evalStmt yes
      else evalStmt no
  While cond loop -> do
    cond' <- evalExpr cond
    when
      cond'
      ( do
          evalStmt loop
          evalStmt stmt
      )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      file <- T.readFile path
      case run path file of
        Left err -> putStrLn err
        Right state -> print state
    [] -> repl
    _ -> putStrLn "Error! Please provide just a filename or no arguments at all."

run :: FilePath -> Text -> Either String State
run name src = case parseStmt name src of
  -- print potential parse errors
  Left errors -> Left $ errorBundlePretty errors
  Right program ->
    -- find all variables and create an init. state
    let vars = findVariables program
        state = foldl (\(State s) var -> State $ Map.insert var 0 s) emptyState vars
        -- run and unpack the interpreter
        (_log, finalState) =
          runInterpreter (evalStmt program)
            & execWriterT
            & flip runStateT state
            & runIdentity
     in Right finalState

repl :: IO ()
repl = do
  src <- T.pack <$> getLine
  case run "src" src of
    Left err -> do
      putStrLn err
      repl
    Right state' -> do
      print state'
      repl -- TODO: keep the new state
