{-# LANGUAGE UndecidableInstances #-}
module Error
  ( module Error
  , module Common.Error
  , module CSE
  , module AE
  ) where

import Common.Error
import qualified Common.Types as T
-- Coroutine
import qualified Coroutine.Stackless.Error as CSE
import qualified Coroutine.Stackless.Types as CST
-- Actor
import qualified Actor.Error as AE
import qualified Actor.Types as AT

data VariantError = InternalError InternalError
                  | CoroutineError (Error CST.Coroutine)
                  | ActorError (Error AT.Actor)

instance Show VariantError where
  show (InternalError e) = show e
  show (CoroutineError e) = show e
  show (ActorError e) = show e

data InternalError = NoVariantFound
                   | UnknownVariant

instance Show InternalError where
  show NoVariantFound = "No language variant found. Start your programs with the form:\n@<variant>"
  show UnknownVariant = "Unknown variation encountered."
