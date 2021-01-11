{-# LANGUAGE TypeFamilies, UndecidableInstances, FlexibleInstances, StandaloneDeriving, AllowAmbiguousTypes #-}
module Common.Types (module Common.Types) where

import qualified Data.Map as Map
import qualified Common.Zipper as Zipper
import qualified Common.Queue as Queue

import Data.List (intercalate)

import Debug.Trace
debug = flip trace

type Pos = (Int,Int)
data Var v = Var { varpos :: Pos, varbase :: String, varindices :: [Expr v] }
instance (Show (Expr v)) => Show (Var v) where
  show (Var p n ids) = n ++ concatMap (\idx -> "[" ++ show idx ++ "]") ids
-- Tables
type TypeTable v      = Map.Map String (VarType v)
type SymbolTable v    = Map.Map String (Value v)
type ProcedureTable v = Map.Map String (Procedure v)

-- Program
data family Program v

-- Procedures
data family VarProcedure v
data Procedure v = ComProcedure (ComProcedure v)
                 | VarProcedure (VarProcedure v)
data ComProcedure v = Function { funcname   :: String
                               , funcparams :: Params v
                               , funcbody   :: [Stmts v]
                               , funcstore :: SymbolTable v
                               , functypes :: TypeTable v
                               }

-- Parameters and Arguments
type Params v = [(String, VarType v)]
type Args     = [String]

-- Statements
type Stmts v = Zipper.Zipper (Stmt v)
data family VarStmt v
data Stmt v = ComStmt (ComStmt v)
            | VarStmt (VarStmt v)
data ComStmt v = Skip
               | Update Pos (Var v) (BinOp v) (Expr v)
               | Swap Pos (Var v) (Var v)
               | Call Pos (Var v) Args
               | Uncall Pos (Var v) Args
               | FunctionReturn
               | Push Pos (Var v) (Var v)
               | Pop  Pos (Var v) (Var v)
               | Enqueue Pos (Var v) (Var v)
               | Dequeue Pos (Var v) (Var v)
               | Unenqueue Pos (Var v) (Var v)
               | Undequeue Pos (Var v) (Var v)
               | Local Pos (VarType v) [String] (Expr v)
               | Delocal Pos (VarType v) [String] (Expr v)
               | If Pos (Expr v) (Stmts v) (Stmts v) (Expr v) Bool
               | Loop Pos (Expr v) (Stmts v) (Stmts v) (Expr v) Bool

instance (Show (BinOp v), Show (Expr v), Show (VarVarType v), Show (Stmt v)) => Show (ComStmt v) where
  show Skip = "skip"
  show (Update _ n op e) = show n ++ " " ++ show op ++ "= " ++ show e
  show (Swap _ n m) = "swap " ++ show n ++ " " ++ show m
  show (Call _ f as) = "call " ++ show f ++ " (" ++ intercalate ", " as ++ ")"
  show (Uncall _ f as) = "uncall " ++ show f ++ " (" ++ intercalate ", " as ++ ")"
  show FunctionReturn = "[function return]"
  show (Push _ n m) = "push " ++ show n ++ " " ++ show m
  show (Pop _ n m) = "pop " ++ show n ++ " " ++ show m
  show (Enqueue _ n m) = "enqueue " ++ show n ++ " " ++ show m
  show (Dequeue _ n m) = "dequeue " ++ show n ++ " " ++ show m
  show (Unenqueue _ n m) = "unenqueue " ++ show n ++ " " ++ show m
  show (Undequeue _ n m) = "undequeue " ++ show n ++ " " ++ show m
  show (Local _ t n e) = "local " ++ show t ++ " " ++ intercalate ", " n ++ " " ++ show e
  show (Delocal _ t n e) = "delocal " ++ show t ++ " " ++ intercalate ", " n ++ " " ++ show e
  show (If _ e1 s1 s2 e2 b) = "if " ++ show e1 ++ " then " ++ show s1 ++ " else " ++ show s2 ++ " fi " ++ show e2
  show (Loop _ e1 s1 s2 e2 b) = "from " ++ show e1 ++ " do " ++ show s1 ++ " loop " ++ show s2 ++ " until " ++ show e2

-- Expressions
data Expr v = BinOp (BinOp v) (Expr v) (Expr v)
            | UnOp  (UnOp v) (Expr v)
            | Value (Value v)

instance (Show (BinOp v), Show (UnOp v), Show (Value v)) => Show (Expr v) where
  show (BinOp op e1 e2) = "(" ++ show e1 ++ ") " ++ show op ++ " (" ++ show e2 ++ ")"
  show (UnOp op e) = show op ++ " (" ++ show e ++ ")"
  show (Value v) = show v

-- Values
data family VarValue v
data Value v = ComValue (ComValue v)
             | VarValue (VarValue v)
data ComValue v = RefVal (Var v)
                | RefFuncVal Pos String
                | QueueVal (Queue.Queue (Value v))
                | ListVal [Value v]
                | IntVal Int
                | ListLit [Expr v]
                | FracVal Int Int
                | BoolVal Int
                | Empty

instance (Show (Value v), Show (Expr v)) => Show (ComValue v) where
  show (RefVal n)       = show n
  show (RefFuncVal _ n) = show n
  show (QueueVal q)     = show q
  show (ListVal l)      = "[" ++ intercalate ", " (map show l) ++ "]"
  show (ListLit l)      = "[" ++ intercalate ", " (map show l) ++ "]"
  show (IntVal v)       = show v
  show (FracVal v1 v2)  = show v1 ++ "/" ++ show v2
  show Empty            = "Ø"

instance (Eq (VarValue v)) => Eq (Value v) where
  (ComValue (RefFuncVal _ v1)) == (ComValue (RefFuncVal _ v2)) = v1 == v2
  (ComValue (IntVal v1)) == (ComValue (IntVal v2)) = v1 == v2
  (ComValue (FracVal v11 v12)) == (ComValue (IntVal v2)) = v11 == v12 * v2
  (ComValue (IntVal v1)) == (ComValue (FracVal v21 v22)) = v22 * v1 == v21
  (ComValue (FracVal v11 v12)) == (ComValue (FracVal v21 v22)) = v11 * v22 == v21 * v12
  (ComValue (ListVal v1)) == (ComValue (ListVal v2)) = v1 == v2
  (ComValue (QueueVal v1)) == (ComValue (QueueVal v2)) = v1 == v2
  (ComValue (ListVal _)) == _ = False
  (ComValue _) == (ComValue (ListVal _)) = False
  (ComValue Empty) == (ComValue Empty) = True
  (VarValue v1) == (VarValue v2) = v1 == v2
  _ == _ = False

-- Unary Operators
data family VarUnOp v
data UnOp v = ComUnOp (ComUnOp v)
            | VarUnOp (VarUnOp v)
data ComUnOp v = Not
               | Size
               | Head
               | Last
               | UnOpEmpty
               deriving (Show)

-- Binary Operators
data family VarBinOp v
data BinOp v = ComBinOp (ComBinOp v)
             | VarBinOp (VarBinOp v)
data ComBinOp v = Plus
                | Minus
                | Times
                | Divide
                | Xor
                | Equal
                | NotEqual
                | Greater
                | Lesser
                | And
                | Or

instance Show (ComBinOp v) where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "/"
  show Xor = "^"
  show Equal = "=="
  show NotEqual = "!="
  show Greater = ">"
  show Lesser = "<"
  show And = "and"
  show Or = "or"

-- Variable Types
data VarType v = ComVarType (ComVarType v)
                | VarVarType (VarVarType v)
data family VarVarType v
data ComVarType v = ListT (VarType v)
                  | QueueT (VarType v)
                  | NumT
                  | FunT [VarType v]
                  | Unknown
instance (Show (VarType v)) => Show (ComVarType v) where
  show (ListT t) = "[" ++ show t ++ "]"
  show (QueueT t) = "[>" ++ show t ++ ">]"
  show NumT      = "num"
  show (FunT ps) = "fun" ++ show ps
  show Unknown   = "*"
deriving instance (Eq (VarType v)) => Eq (ComVarType v)

instance (Show (VarVarType v)) => Show (VarType v) where
  show (ComVarType t) = show t
  show (VarVarType t) = show t

instance (Eq (VarVarType v)) => Eq (VarType v) where
  (ComVarType t1) == (ComVarType t2) = t1 == t2
  (VarVarType t1) == (VarVarType t2) = t1 == t2
  _ == _ = False

type TypeTab v = Map.Map String (Value v)
