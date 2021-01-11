{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Coroutine.Stackless.Inverter (module Coroutine.Stackless.Inverter, module Common.Inverter, module Common.Classes) where

import Common.Classes
import Common.Inverter
import Coroutine.Stackless.Types
import qualified Common.Zipper as Zipper

instance VariantInverter Coroutine where
  inverse p = Right $ p { progbody = inverseProcedure (progbody p) }

  inverseVarProcedure c = c { corobody = inverseBody (corobody c) }
    where inverseBody layers
            | length layers > 1 =
              let l:ls = layers in (Zipper.reversez . Zipper.mapz inverseStmt) l:map (Zipper.left . Zipper.reversez . Zipper.mapz inverseStmt) ls
            | otherwise = map (Zipper.reversez . Zipper.mapz inverseStmt) layers

  inverseVarStmt (Spawn p l f q as)   = Despawn p l f q as
  inverseVarStmt (Despawn p l f q as) = Spawn p l f q as
  inverseVarStmt (Resume p n as)      = Unresume p n as
  inverseVarStmt (Unresume p n as)    = Resume p n as
  inverseVarStmt stmt                 = stmt
  inverseVarBinOp op                  = op  
