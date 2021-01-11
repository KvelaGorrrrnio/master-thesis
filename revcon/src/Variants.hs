module Variants 
  (module Variants
  , CST.Coroutine
  , AT.Actor
  ) where

import qualified Coroutine.Stackless.Types as CST
import qualified Actor.Types as AT

data Variant = CoroutineVariant (CST.Program CST.Coroutine)
             | ActorVariant (AT.Program AT.Actor)
             deriving (Show)
