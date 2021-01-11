module Static where

import qualified Coroutine.Stackless.Static as CSTC
import qualified Actor.Static as ATC
import Common.Types
import Variants
import Error

typecheck :: Either VariantError Variant -> Either VariantError Variant
typecheck (Left e) = Left e
typecheck (Right (CoroutineVariant p)) = case CSTC.staticcheck p of
                                           Left e -> Left . CoroutineError . TypeError $ e
                                           Right p' -> Right . CoroutineVariant $ p'
typecheck (Right (ActorVariant p))     = case ATC.staticcheck p of
                                           Left e -> Left . ActorError . TypeError $ e
                                           Right p' -> Right . ActorVariant $ p'
