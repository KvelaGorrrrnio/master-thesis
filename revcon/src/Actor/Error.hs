{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Actor.Error (module Actor.Error, module Common.Error) where
  
import Common.Error
import Actor.Types

-- Parser Errors
type instance VarParseError Actor = ActorParseError
data ActorParseError = MultipleMainInits
                     | NoMainInitFound
                     | NoMainFound

instance Show ActorParseError where
  show MultipleMainInits = "Multiple main.init was found.\nOnly one main.init must be defined."
  show NoMainInitFound = "No main.init actor was found."
  show NoMainFound = "No main actor was found."

-- Typecheck Errors
type instance VarTypeError Actor = ()

-- Interpreter Errors
type instance VarInterpreterError Actor = ActorInterpreterError
data ActorInterpreterError = WrongMessage (ActorId,ActorId,String) (ActorId,ActorId,String)
                           | NoMessages
                           | UnknownActorId ActorId
                           | UnknownMailpoint Pos String String

instance Show ActorInterpreterError where
  show (WrongMessage e g) = "Wrong message received, expected message " ++ show e ++ ", but got " ++ show g ++ "."
  show NoMessages = "No messages was found."
  show (UnknownMailpoint pos a mp) = "Unknown mailpoint " ++ a ++ "." ++ mp ++ " at " ++ show pos ++ "."
  show (UnknownActorId id) = "Unknown actor ID " ++ show id ++ "."

-- Inverter Errors
type instance VarInverterError Actor = ()
