{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Actor.Inverter (module Actor.Inverter, module Common.Inverter) where

import Common.Classes
import Common.Inverter
import Actor.Types
import qualified Common.Zipper as Zipper

instance VariantInverter Actor where
  -- TODO: Make reversible (think about how the program actually needs to reversed)
  inverse p = Right $ p { progbody = inverseProcedure (progbody p) }

  inverseVarProcedure c = c { actobody = inverseBody (actobody c) }
    where inverseBody layers
            | length layers > 1 =
              let l:ls = layers in (Zipper.reversez . Zipper.mapz inverseStmt) l:map (Zipper.left . Zipper.reversez . Zipper.mapz inverseStmt) ls
            | otherwise = map (Zipper.reversez . Zipper.mapz inverseStmt) layers

  inverseVarStmt (Spawn p l f) = Despawn p l f
  inverseVarStmt (Despawn p l f) = Spawn p l f
  inverseVarStmt (Send p a mp as) = Unsend p a mp as
  inverseVarStmt (Unsend p a mp as) = Send p a mp as
  inverseVarStmt (Deliver p m) = Undeliver p m
  inverseVarStmt (Undeliver p m) = Deliver p m

  inverseVarBinOp op = op
