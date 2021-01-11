{-# LANGUAGE TypeFamilies, FlexibleInstances, UndecidableInstances, StandaloneDeriving #-}
module Common.Error where

import qualified Text.Parsec.Error as Parsec (ParseError)
import Common.Types
import Data.List (intercalate)
import qualified Data.Map as Map (toList)

data Error v = ParseError (ParseError v)
             | TypeError  (TypeError v)
             | InterpreterError (InterpreterError v)
             | InverterError (InverterError v)

instance (Show (ParseError v)
        , Show (TypeError v)
        , Show (InterpreterError v)
        , Show (InverterError v)) => Show (Error v) where
  show (ParseError e)       = show e
  show (TypeError e)        = show e
  show (InterpreterError e) = show e
  show (InverterError e)    = show e

-- Parse Errors
data ParseError v = ComParseError ComParseError
                  | VarParseError (VarParseError v)
type family VarParseError v
type ComParseError = Parsec.ParseError

instance (Show (VarParseError v)) => Show (ParseError v) where
  show (ComParseError e) = show e
  show (VarParseError e) = show e

-- Type Errors
data TypeError v = ComTypeError (ComTypeError v)
                 | VarTypeError (VarTypeError v)
                 | ColTypeError [TypeError v]
type family VarTypeError v
type ComTypeError v = CommonTypeError v

data CommonTypeError v = UnexpectedType (Var v) (VarType v) (VarType v)
                       | UnexpectedExprType Pos (VarType v) (VarType v)
                       | UnexpectedUnOpType (UnOp v) (VarType v) (VarType v) (VarType v)
                       | UnexpectedBinOpType (BinOp v) (VarType v,VarType v) (VarType v) (VarType v,VarType v)
                       | TooManyIndices (Var v)
                       | UsedBeforeInitialized Pos (Var v)
                       | IncorrectArgNumber (Var v) Int Int
                       | IncomparableTypes (BinOp v) (VarType v) (VarType v)
                       | IllegalProcedure String (Stmt v)

instance (Show (Stmt v), Show (UnOp v), Show (BinOp v), Show (Value v), Show (VarVarType v)) => Show (CommonTypeError v) where
  show (UnexpectedType n t1 t2) = "Expected '" ++ show n ++ "' to be of type '" ++ show t1 ++ "' instead of actual type '" ++ show t2 ++ "'."
  show (UnexpectedExprType p t1 t2) = "Expected expression to be of type '" ++ show t1 ++ "' instead of actual type '" ++ show t2 ++ "' at " ++ show p ++ "."
  show (UnexpectedBinOpType op (t11,t12) t (t21,t22)) = "Operator " ++ show op ++ " has type " ++ show t11 ++ " -> " ++ show t12 ++ " -> " ++ show t ++ ", but is used as " ++ show t21 ++ " -> " ++ show t22 ++ " -> " ++ show t ++ "."
  show (UnexpectedUnOpType op t1 t t2) = "Operator " ++ show op ++ " has type " ++ show t1 ++ " -> " ++ show t ++ ", but is used as " ++ show t2 ++ " -> " ++ show t ++ "."
  show (TooManyIndices n) = "'" ++ show n ++ "' was accessed with too many indices."
  show (UsedBeforeInitialized p n) = "'" ++ show n ++ "' was used before being initialized at " ++ show p ++ "."
  show (IncorrectArgNumber m n1 n2) = "'" ++ show m ++ "' was called with wrong number of arguments. Expected " ++ show n1 ++ ", but got " ++ show n2 ++ "."
  show (IncomparableTypes op t1 t2) = "The types '" ++ show t1 ++ "' and '" ++ show t2 ++ "' can't be compared with the " ++ show op ++ " operator."
  show (IllegalProcedure f s) = "Illegal procedure '" ++ f ++ "' referenced in " ++ show s ++ "."


instance (Show (ComTypeError v), Show (VarTypeError v)) => Show (TypeError v) where
  show (ComTypeError e) = show e
  show (VarTypeError e) = show e
--  show (ColTypeError e) = concatMap show e
  show (ColTypeError e) = intercalate "\n" (map show e)

-- Interpret Errors
data InterpreterError v = ComInterpreterError (ComInterpreterError v)
                      | VarInterpreterError (VarInterpreterError v)
type family VarInterpreterError v
data ComInterpreterError v = AssertionFailed (Expr v) Bool Bool
                           | NoMoreStatements String
                           | UnknownVariable (Var v)
                           | NonEmpty (Var v) (Value v)
                           | DelocalUnexpectedValue String (Value v) (Value v)
                           | NonListValue (Var v) (Value v)
                           | NonQueueValue (Var v) (Value v)
                           | EmptyListValue (Var v)
                           | EmptyQueueValue (Var v)
                           | NoReturnLayer
                           | UnknownProcedure String
                           | NonEmptyLocalStore String (SymbolTable v)
                           | InconsistentArgs String (SymbolTable v) (SymbolTable v)
                           | NonLayeredStmt (Stmt v)
                           | SelfReferencing (Var v) (Stmt v)
                           | MultiReferencingArgs Pos [String]
                           | ZeroMultiplication (Stmt v)
                           | ZeroDivision (Stmt v)
                           | NoLayers
                           | EmptyStack
                           | EmptyListOp (UnOp v)
                           | ListOutOfBounds (Value v) Int
                           | NonIntegerIndex (Value v) (Value v)
                           | NonListIndex (Value v) [Value v]

instance (Show (ComInterpreterError v), Show (VarInterpreterError v)) => Show (InterpreterError v) where
  show (ComInterpreterError e) = show e
  show (VarInterpreterError e) = show e

instance (Show (VarType v), Show (Stmt v), Show (Expr v), Show (Value v), Show (UnOp v)) => Show (ComInterpreterError v) where
  show (AssertionFailed e bt bf) = "Expected " ++ show e ++ " to be " ++ show bt ++ ", but was " ++ show bf ++ "."
  show (NoMoreStatements n) = "The end of the procedure "++n++" was unexpectedly reached."
  show (UnknownVariable n) = "Unknown variable " ++  show n ++ "."
  show (NonEmpty n v) = "'" ++ show n ++ "' was expected to be empty, but has value " ++ show v ++ "."
  show (DelocalUnexpectedValue n vt vf) = "Failed deallocating '" ++ n ++ "'. Expected it to have the value " ++ show vt ++ ", but has " ++ show vf ++ "."
  show (NonListValue n v) = "'" ++ show n ++ "' was expected to be a list, but has the value " ++ show v ++ "."
  show (NonQueueValue n v) = "'" ++ show n ++ "' was expected to be a queue, but has the value " ++ show v ++ "."
  show (EmptyListValue n) = "Expected '" ++ show n ++ "' to be a non-empty list, but was empty."
  show (EmptyQueueValue n) = "Expected '" ++ show n ++ "' to be a non-empty queue at " ++ show (varpos n) ++ ", but was empty."
  show (UnknownProcedure n) = "Unknown procedure '" ++ n ++ "' encountered."
  show (NonEmptyLocalStore n sym) = "The local store of " ++ n ++ " was expected to be empty, but has the following non-empty entries:\n" ++ concatMap (\(k,v) -> "  " ++ k ++ ": " ++ show v) (Map.toList sym)
  show (InconsistentArgs n a b) = "The arguments for " ++ n ++ " are inconsistent. Expected to be:\n" ++ show a ++ "\nBut got:\n" ++ show b
  show (NonLayeredStmt s) = "Encountered non layered stmt '" ++ show s ++ "'."
  show (SelfReferencing n s) = "The variable " ++ show n ++ " has been self-referenced in the statement: '" ++ show s ++ "'"
  show (MultiReferencingArgs p as) = "The variable" ++ (if length as > 1 then "s" else "") ++ " " ++ intercalate ", " as ++ " was referenced multiple times as args at " ++ show p ++ "."
  show (ZeroMultiplication s) = "An attempt to destructively make a zero multiplication in " ++ show s ++ "."
  show (ZeroDivision s) = "An attempt to destructively make a zero division in " ++ show s ++ "."
  show NoLayers = "THere are no layers to retrieve."
  show EmptyStack = "Empty stack encountered."
  show (EmptyListOp op) = "Can't perform " ++ show op ++ " on an empty list."
  show (ListOutOfBounds v n) = "Can't index into value " ++ show v ++ " with " ++ show n ++ "."
  show (NonIntegerIndex v idx) = "Can't index into value " ++ show v ++ " with non-integer value " ++ show idx ++ "."
  show (NonListIndex v ids) = "Can't index on non-list value " ++ show v ++ " with indice(s) " ++ show ids ++ "."

data InverterError v = ComInverterError (ComInverterError v)
                     | VarInverterError (VarInverterError v)
type family VarInverterError v
data ComInverterError v

instance (Show (ComInverterError v), Show (VarInverterError v)) => Show (InverterError v) where
  show (ComInverterError e) = show e
  show (VarInverterError e) = show e

deriving instance Show (ComInverterError v)
