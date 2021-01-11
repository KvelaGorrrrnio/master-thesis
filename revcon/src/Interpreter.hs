module Interpreter (Interpreter.interpret) where

import qualified Coroutine.Stackless.Interpreter as CSI
import qualified Actor.Interpreter as AI
import Variants
import Error


interpret :: Either VariantError Variant ->  Either VariantError Variant
interpret (Left err) = Left err
interpret (Right (CoroutineVariant p)) = 
    case CSI.interpret p of
        Left e -> Left . CoroutineError . InterpreterError $ e
        Right p' -> Right . CoroutineVariant $ p'
interpret (Right (ActorVariant p)) = 
    case CSI.interpret p of
        Left e -> Left . ActorError . InterpreterError $ e
        Right p' -> Right . ActorVariant $ p'
