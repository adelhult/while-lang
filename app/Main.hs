{-# LANGUAGE GADTs #-}

module Main where

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
      map (\(k, v) -> k ++ ": " ++ show v) (Map.toList state)

emptyState :: State
emptyState = State Map.empty

getVar :: Variable -> State -> Maybe Number
getVar x (State state) = Map.lookup x state

assignVar :: Variable -> Number -> State -> State
assignVar x value (State state) = State $ Map.insert x value state

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

-- TODO: Would be nice to use a State and Except monad transformer
evalExpr :: Expr a -> State -> a
evalExpr expr state = case expr of
  -- Arithmetic expressions
  NumLit n -> n
  Var x -> case getVar x state of
    Just n -> n
    Nothing -> error $ "Variable '" ++ x ++ "' not found"
  Add lhs rhs -> evalExpr lhs state + evalExpr rhs state
  Mul lhs rhs -> evalExpr lhs state * evalExpr rhs state
  Sub lhs rhs -> evalExpr lhs state - evalExpr rhs state
  -- Boolean expressions
  TrueLit -> True
  FalseLit -> False
  Eq lhs rhs -> evalExpr lhs state == evalExpr rhs state
  LessEq lhs rhs -> evalExpr lhs state <= evalExpr rhs state
  And lhs rhs -> evalExpr lhs state && evalExpr rhs state
  Neg b -> not (evalExpr b state)

evalStmt :: Stmt -> State -> State
evalStmt stmt state = case stmt of
  Assign x value -> assignVar x (evalExpr value state) state
  Skip -> state
  Sequence ss -> foldl (flip evalStmt) state ss
  If cond yes no ->
    if evalExpr cond state
      then evalStmt yes state
      else evalStmt no state
  While cond loop ->
    if evalExpr cond state
      -- run the loop then repeat while stmt again using the new state
      then evalStmt stmt (evalStmt loop state)
      -- or just do nothing
      else state

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
        state = foldl (\s var -> assignVar var 0 s) emptyState vars
     in -- finally evaluate the program and print the resulting state
        Right $ evalStmt program state

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
