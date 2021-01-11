{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances #-}
module Coroutine.Stackless.Types (module Coroutine.Stackless.Types, module Common.Types) where

import Common.Types
import Common.Classes
import qualified Data.Map as Map
import qualified Data.List as List (intercalate)

-- Program
type CoroutineId = Int
data Coroutine = CoroutineVariantReference deriving Show
data instance Program Coroutine =
    Program { progname   :: String
            , progbody   :: Procedure Coroutine
            , coroutines :: Map.Map CoroutineId (Procedure Coroutine)
            , progstack  :: [Procedure Coroutine]
            , procedures :: ProcedureTable Coroutine
            }
instance (ProcedureInfo Coroutine) where
  name (VarProcedure c) = coroname c
  name (ComProcedure f) = funcname f
  store (VarProcedure c) = corostore c
  store (ComProcedure f) = funcstore f
  types (VarProcedure c) = corotypes c
  types (ComProcedure f) = functypes f
  params (VarProcedure c) = coroparams c
  params (ComProcedure f) = funcparams f
  body (VarProcedure c) = corobody c
  body (ComProcedure f) = funcbody f

instance Show (Program Coroutine) where
  show p = progname p ++ ":" ++ show (progbody p)

-- Coroutine
data instance VarProcedure Coroutine = Coroutine { coroname         :: String
                                                 , coroid           :: CoroutineId
                                                 , corolabel        :: String
                                                 , coroparams       :: Params Coroutine
                                                 , corostaticparams :: Params Coroutine
                                                 , corobody         :: [Stmts Coroutine]
                                                 , corostore        :: SymbolTable Coroutine
                                                 , corotypes        :: TypeTable Coroutine
                                                 }
deriving instance Show (ComProcedure Coroutine)
instance Show (VarProcedure Coroutine) where
  show c = coroname c ++ if corolabel c /= "" then" (" ++ coroname c ++ ")" else "" ++ "\n[store]\n" ++ List.intercalate "\n" (map (\(k,v)-> "  " ++ k ++ ": " ++ show v) (Map.toList $ corostore c)) ++ "\n\n[stack]" ++ List.intercalate "\n------\n" (map show (corobody c))

instance Show (Procedure Coroutine) where
  show (ComProcedure p) = show p
  show (VarProcedure p) = show p

-- Statements
data instance VarStmt Coroutine = Spawn Pos String String String Args
                                | Despawn Pos String String String Args
                                | Resume Pos String Args
                                | Yield Pos String
                                | Unresume Pos String Args

instance Show (VarStmt Coroutine) where
  show (Spawn _ l f q as) = "spawn " ++ l ++ " " ++ f ++ " " ++ q ++ " (" ++ List.intercalate ", " as ++ ")"
  show (Despawn _ l f q as) = "despawn " ++ l ++ " " ++ f ++ " " ++ q ++ " (" ++ List.intercalate ", " as ++ ")"
  show (Resume _ n as) = "resume " ++ n ++ " (" ++ List.intercalate ", " as ++ ")"
  show (Unresume _ n as) = "unresume " ++ n ++ " (" ++ List.intercalate ", " as ++ ")"
  show (Yield _ q) = "yield " ++ q

instance Show (Stmt Coroutine) where
  show (ComStmt s) = show s
  show (VarStmt s) = show s

-- Values
data instance VarValue Coroutine = CoroutineVal String CoroutineId [VarType Coroutine]
instance Show (VarValue Coroutine) where
  show (CoroutineVal n _ ts) = n ++ " (" ++ List.intercalate ", " (map show ts) ++ ")"
instance Eq (VarValue Coroutine) where
  (CoroutineVal _ i1 ts1) == (CoroutineVal _ i2 ts2) = i1 == i2 && ts1 == ts2
instance Show (Value Coroutine) where
  show (ComValue v) = show v
  show (VarValue v) = show v

-- Unary Operators
data instance VarUnOp Coroutine
deriving instance Show (VarUnOp Coroutine)
instance Show (UnOp Coroutine) where
  show (ComUnOp op) = show op
  show (VarUnOp op) = show op

-- Binary Operators 
data instance VarBinOp Coroutine
deriving instance Show (VarBinOp Coroutine)
instance Show (BinOp Coroutine) where
  show (ComBinOp op) = show op
  show (VarBinOp op) = show op

-- Variable Types
data instance VarVarType Coroutine = CoroutineT [VarType Coroutine] deriving (Show, Eq)
