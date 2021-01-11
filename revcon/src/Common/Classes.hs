{-# LANGUAGE FlexibleContexts, TypeFamilies, AllowAmbiguousTypes #-}
module Common.Classes where

import Common.Types hiding (ParseError)
import Common.Error

import qualified Text.Parsec.Indent as Indent (IndentParserT)
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Except as Except
import qualified Data.Map as Map
import qualified Common.Error as Error

type Parser v a = Indent.IndentParserT String () (Except.Except (Error.ParseError v)) a


class VariantParser v where
  parse :: String -> String -> Either (ParseError v) (Program v)
  parseProgram      :: String -> Parser v (Program v)
  parseVarProcedure :: Parser v (String, Procedure v)
  parseVarStmt      :: Parser v (Stmt v)
  parseVarValue     :: Parser v (Value v)
  parseVarUnOp      :: Parser v (UnOp v)
  parseVarBinOp     :: Int -> Parser v (BinOp v)
  parseVarType      :: Parser v (VarType v)

class (Eq (VarVarType v)) => TypedVariant v where
  getSymbols        :: Procedure v -> SymbolTable v
  setSymbols        :: Procedure v -> SymbolTable v -> Procedure v
  settype           :: String -> VarType v -> TypeTable v -> TypeTable v
  gettype           :: String -> TypeTable v -> Maybe (VarType v)
  varVarType2symbol :: VarVarType v -> Value v
  compareVarTypes   :: VarVarType v -> VarType v -> Maybe (VarType v)

type TypeCheckState v = State.StateT ([TypeError v],TypeTable v) (Reader.Reader (Program v))

class (TypedVariant v,Show (VarVarType v), Show (Expr v), Show (BinOp v), Show (VarStmt v), Show(Stmt v)) => VariantTypeChecker v where
  staticcheck :: Program v -> Either (TypeError v) (Program v)

  settypes              :: Program v -> Either (TypeError v) (Program v)
  settypesVarProcedure  :: VarProcedure v -> ProcedureTable v -> VarProcedure v
  settypesVarStmt       :: VarStmt v -> State.State (TypeTable v, ProcedureTable v) ()

  typecheck             :: Program v  -> Either (TypeError v) (Program v)
  typecheckVarProcedure :: VarProcedure v -> TypeCheckState v ()
  typecheckVarStmt      :: VarStmt v  -> TypeCheckState v ()
  typecheckVarValue     :: VarValue v -> TypeCheckState v (VarType v)
  typecheckVarBinOp     :: VarBinOp v -> VarType v -> VarType v -> TypeCheckState v (VarType v)
  typecheckVarUnOp      :: VarUnOp v -> VarType v -> TypeCheckState v (VarType v)

  lookupProc :: String -> TypeCheckState v (Maybe (Procedure v))
  typeofVarProc :: VarProcedure v -> TypeCheckState v (VarType v)

class (ProcedureInfo v) where
  name   :: Procedure v -> String
  store  :: Procedure v -> SymbolTable v
  types  :: Procedure v -> TypeTable v
  params :: Procedure v -> Params v
  body   :: Procedure v -> [Stmts v]

type InterpreterState v = State.StateT (Program v) (Except.Except (InterpreterError v))

class (Show (VarStmt v), Show (ComStmt v), Show (Program v), Show (Stmt v), Show (Value v), Show (BinOp v), Show (UnOp v), Show (Procedure v)
      , Eq (VarValue v)
      , ProcedureInfo v
      , VariantInverter v
      , VariantTypeChecker v) => VariantInterpreter v where
  interpret        :: Program v -> Either (InterpreterError v) (Program v)
  interpretProgram :: InterpreterState v ()
  interpretVarStmt :: VarStmt v -> InterpreterState v ()

  evalVarBinOp      :: VarBinOp v -> Value v -> Value v -> Bool -> InterpreterState v (Value v)
  evalVarUnOp       :: VarUnOp v -> Value v -> InterpreterState v (Value v)
  evalVarValue      :: VarValue v -> InterpreterState v (Value v)

  returnVarLayer    :: VarStmt v -> Stmts v -> InterpreterState v ()

  selfReferencingVarValue :: Var v -> VarValue v -> InterpreterState v ()
  emptyValue :: Var v -> InterpreterState v (Value v)

  -- Layers
  addLayer          :: Stmts v -> InterpreterState v ()
  getLayers         :: Procedure v -> InterpreterState v (Stmts v, [Stmts v])
  getCurrentLayers  :: InterpreterState v (Stmts v, [Stmts v])
  setLayers         :: Procedure v -> [Stmts v] -> InterpreterState v (Procedure v)
  setCurrentLayers  :: [Stmts v] -> InterpreterState v ()

  -- Stack
  getEnv   :: InterpreterState v (Procedure v)
  setEnv   :: Procedure v -> InterpreterState v ()
  getStack :: InterpreterState v [Procedure v]
  setStack :: [Procedure v] -> InterpreterState v ()

  -- Utilities
  getStore          :: InterpreterState v (SymbolTable v)
  setStore          :: SymbolTable v -> InterpreterState v ()
  getProcedureTable :: InterpreterState v (ProcedureTable v)
  getStmt           :: InterpreterState v (Maybe (Stmt v))
  nextStmt          :: InterpreterState v ()
  prevStmt          :: InterpreterState v ()
  insertStmt        :: Stmt v -> InterpreterState v ()
  removeStmt        :: InterpreterState v (Maybe (Stmt v))

class VariantInverter v where
  inverse  :: Program v -> Either (InverterError v) (Program v)
  inverseVarProcedure :: VarProcedure v -> VarProcedure v
  inverseVarStmt  :: VarStmt v -> VarStmt v
  inverseVarBinOp :: VarBinOp v -> VarBinOp v
