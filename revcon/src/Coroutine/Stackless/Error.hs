{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Coroutine.Stackless.Error (module Coroutine.Stackless.Error, module Common.Error) where
  
import Common.Error
import Coroutine.Stackless.Types

-- Parser Errors
type instance VarParseError Coroutine = CoroutineParseError
data CoroutineParseError = MultipleMains
                         | NoMainFound

instance Show CoroutineParseError where
  show MultipleMains = "Multiple coroutines named main was found.\nOnly one main must be defined."
  show NoMainFound = "No main coroutine was found."

-- Typecheck Errors
type instance VarTypeError Coroutine = CoroutineTypeCheckError
data CoroutineTypeCheckError = WrongCoroutine String String String
                             | CoroutineNotFound String

instance Show CoroutineTypeCheckError where
  show (WrongCoroutine n f f') = show n ++ " was expected to be a coroutine instance of " ++ f ++ " but is an instance of " ++ f' ++ "."
  show (CoroutineNotFound f)   = "The coroutine " ++ f ++ " was not found."

-- Interpreter Errors
type instance VarInterpreterError Coroutine = CoroutineInterpreterError
data CoroutineInterpreterError = NotAtBreakpoint Pos
                               | UnexpectedBreakpoint String String
                               | BreakpointNotFound String
                               | UnknownCoroutine String
                               | UnknownCoroutineId Pos CoroutineId
                               | UnexpectedCoroutineName String String
                               | ExpectedCoroutine (Procedure Coroutine)
                               | YieldedFromNonCoroutine Pos

instance Show CoroutineInterpreterError where
  show (NotAtBreakpoint p)    = "Not at breakpoint " ++ show p ++ "."
  show (UnexpectedBreakpoint qt qf) = "Expected to be at breakpoint " ++ qt ++ ", but is at " ++ qf ++ "."
  show (UnknownCoroutine n)   = "Unknown coroutine '" ++ n ++ "' encountered."
  show (UnknownCoroutineId p v)   = "Unknown coroutine identifer '" ++ show v ++ "' encountered at " ++ show p ++ "."
  show (BreakpointNotFound n) = "The breakpoint '" ++ n ++ "' was not found."
  show (UnexpectedCoroutineName n m) = "Expected coroutine to be a " ++ n ++ " instance, but is an instance of " ++ m ++ "."
  show (YieldedFromNonCoroutine p) = "Trying to yield from non-coroutine procedure at " ++ show p ++ "."


-- Inverter Errors
type instance VarInverterError Coroutine = ()
