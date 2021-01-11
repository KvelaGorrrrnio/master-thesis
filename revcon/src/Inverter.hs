module Inverter (Inverter.invert) where

import qualified Coroutine.Stackless.Inverter as CSI
import qualified Actor.Inverter as AI
import Variants
import Error

invert :: Either VariantError Variant ->  Either VariantError Variant
invert (Left err) = Left err
invert (Right (CoroutineVariant p)) = 
    case CSI.inverse p of
        Left e -> Left . CoroutineError . InverterError $ e
        Right p' -> Right . CoroutineVariant $ p'
invert (Right (ActorVariant p)) = 
    case CSI.inverse p of
        Left e -> Left . ActorError . InverterError $ e
        Right p' -> Right . ActorVariant $ p'
