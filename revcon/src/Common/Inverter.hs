module Common.Inverter where

import Common.Classes
import Common.Types
import qualified Common.Zipper as Zipper

inverseProcedure :: (VariantInverter v) => Procedure v -> Procedure v
inverseProcedure (ComProcedure p) = ComProcedure $ inverseComProcedure p
inverseProcedure (VarProcedure p) = VarProcedure $ inverseVarProcedure p

inverseComProcedure :: (VariantInverter v) => ComProcedure v -> ComProcedure v
inverseComProcedure f = f { funcbody = inverseBody (funcbody f) }
  where inverseBody layers
          | length layers > 1 =
            let l:ls = layers in (Zipper.reversez . Zipper.mapz inverseStmt) l:map (Zipper.left . Zipper.reversez . Zipper.mapz inverseStmt) ls
          | otherwise = map (Zipper.reversez . Zipper.mapz inverseStmt) layers

inverseStmts :: (VariantInverter v) => Stmts v -> Stmts v
inverseStmts = Zipper.reversez . Zipper.mapz inverseStmt

inverseStmt :: (VariantInverter v) => Stmt v -> Stmt v
inverseStmt (ComStmt s) = ComStmt $ inverseComStmt s
inverseStmt (VarStmt s) = VarStmt $ inverseVarStmt s

inverseComStmt :: (VariantInverter v) => ComStmt v -> ComStmt v
inverseComStmt (Update p n op e) = Update p n (inverseBinOp op) e
inverseComStmt (If p e1 s1 s2 e2 b) = If p e2 (Zipper.reversez $ Zipper.mapz inverseStmt s1) (Zipper.reversez $ Zipper.mapz inverseStmt s2) e1 b
inverseComStmt (Loop p e1 s1 s2 e2 b) = Loop p e2 (Zipper.reversez $ Zipper.mapz inverseStmt s1) (Zipper.reversez $ Zipper.mapz inverseStmt s2) e1 b
inverseComStmt (Call p s a)      = Uncall p s a
inverseComStmt (Uncall p s a)    = Call p s a
inverseComStmt (Push p n m)      = Pop p n m
inverseComStmt (Pop p n m)       = Push p n m
inverseComStmt (Enqueue p n m)   = Unenqueue p n m
inverseComStmt (Unenqueue p n m) = Enqueue p n m
inverseComStmt (Dequeue p n m)   = Undequeue p n m
inverseComStmt (Undequeue p n m) = Dequeue p n m
inverseComStmt (Local p t n e)   = Delocal p t n e
inverseComStmt (Delocal p t n e) = Local p t n e
inverseComStmt s = s

inverseBinOp :: (VariantInverter v) => BinOp v -> BinOp v
inverseBinOp (ComBinOp op) = ComBinOp $ inverseComBinOp op
inverseBinOp (VarBinOp op) = VarBinOp $ inverseVarBinOp op

inverseComBinOp :: (VariantInverter v) => ComBinOp v -> ComBinOp v
inverseComBinOp Plus   = Minus
inverseComBinOp Minus  = Plus
inverseComBinOp Times  = Divide
inverseComBinOp Divide = Times
inverseComBinOp op     = op
