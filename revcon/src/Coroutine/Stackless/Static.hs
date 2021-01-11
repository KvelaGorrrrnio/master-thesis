{-# LANGUAGE TypeFamilies, UndecidableInstances, FlexibleContexts, LambdaCase #-}
module Coroutine.Stackless.Static (module Coroutine.Stackless.Static, module Common.Static, module Common.Classes) where

import Coroutine.Stackless.Types
import Coroutine.Stackless.Error
import Common.Classes
import Common.Static
import Error
import qualified Common.Zipper as Zipper

import qualified Data.Maybe as Maybe (fromJust, mapMaybe)
import qualified Data.Map as Map
import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader
import qualified Data.Bifunctor (second)

instance TypedVariant Coroutine where
  getSymbols = store
  setSymbols (VarProcedure p) s = VarProcedure $ p { corostore = s }
  setSymbols (ComProcedure p) s = ComProcedure $ p { funcstore = s }
  settype n t m = Map.insert n t m
  gettype n m   = Map.lookup n m

  varVarType2symbol (CoroutineT _) = ComValue Empty

  compareVarTypes (CoroutineT n) (VarVarType (CoroutineT m))
    | n == m    = Just . VarVarType $ CoroutineT n
    | otherwise = Nothing
  compareVarTypes (CoroutineT n) (ComVarType Unknown) = Just . VarVarType $ CoroutineT n
  compareVarTypes _ _ = Nothing

-- Main part
instance VariantTypeChecker Coroutine where
  -- Static check
  staticcheck p = settypes p >>= typecheck >>= initiateStores
    where initiateStores p = Right . setStartup $ p { procedures = Map.map initiateProcedureStore (procedures p) }
          initiateProcedureStore (VarProcedure c) = VarProcedure $ c { corostore = Map.map (\_ -> ComValue Empty) (corotypes c) }
          initiateProcedureStore (ComProcedure p) = ComProcedure $ p { funcstore = Map.map (\_ -> ComValue Empty) (functypes p) }
          
          setStartup prog@(Program { progbody = (VarProcedure c) }) = prog { progbody = VarProcedure initialBody, coroutines = Map.fromList [(0,VarProcedure initialBody)] }
            where initialBody = c { coroid = 0, corostore = initialStore main, corobody = [initialStmts main] }
                  main = Maybe.fromJust (Map.lookup "main" (procedures prog))
                  initialStore (VarProcedure m) = Map.union (Map.fromList [("main", ComValue Empty)]) (Map.map type2symbol (Map.fromList $ coroparams m))
                  initialStmts (VarProcedure m) = Zipper.fromList [ VarStmt (Spawn   (1,1) "main" "main" "begin" [])
                                                                  , VarStmt (Resume  (2,1) "main" (map fst $ coroparams m))
                                                                  , VarStmt (Despawn (3,1) "main" "main" "end" [])
                                                                  ]

  -- Set types
  settypes p = Right p { procedures = Map.map (`settypesProcedure` procedures p) (procedures p) }

  -- Procedures (Coroutine)
  settypesVarProcedure c pt = c { corotypes = fst $ State.execState (settypesLayers (corobody c)) (Map.fromList (coroparams c ++ corostaticparams c), pt) }

  -- Spawn
  settypesVarStmt (Spawn _ n f _ _) = do
    (m,pt) <- State.get
    case gettype n m of
      Nothing ->
        case Map.lookup f pt of
          Just (VarProcedure c) -> State.put (settype n (VarVarType $ CoroutineT (map snd $ coroparams c)) m,pt)
          _ -> return ()
      Just _  -> return ()

  -- Despawn
  settypesVarStmt (Despawn _ n f _ _) = do
    (m,pt) <- State.get
    case gettype n m of
      Nothing -> 
        case Map.lookup f pt of
          Just (VarProcedure c) -> State.put (settype n (VarVarType $ CoroutineT (map snd $ coroparams c)) m,pt)
          _ -> return ()
      Just _  -> return ()

  -- rest
  settypesVarStmt _ = return ()

  -- Type check
  typecheck p = case Reader.runReader (State.execStateT (foldr ((>>) . typecheckProcedure) (return ()) (Map.elems $ procedures p)) ([],Map.empty)) p of
                   ([],_) -> Right p
                   (es,_) -> Left $ ColTypeError es

  -- Procedures (Coroutine)
  typecheckVarProcedure c = withTypeTable (corotypes c) >> typecheckLayers (corobody c) >> withTypeTable Map.empty
    where withTypeTable t = State.get >>= \(s,_) -> State.put (s,t)
          typecheckLayers ls = foldr ((>>) . typecheckStmts) (return ()) ls

  -- Spawn / Despawn
  typecheckVarStmt (Despawn p n f e s) = typecheckVarStmt (Spawn p n f e s)
  typecheckVarStmt s@(Spawn _ _ "main" _ _) = addError (ComTypeError $ IllegalProcedure "main" (VarStmt s))
  typecheckVarStmt (Spawn p n f _ ss) = do
    tn <- typeof (Var p n [])
    tf <- typeofProc f
    case (tf,tn) of
      (VarVarType (CoroutineT _), VarVarType (CoroutineT _)) ->
        getstaticparams f >>= \case
          Just ps -> if length ps /= length ss
                        then addError (ComTypeError $ IncorrectArgNumber (Var p f []) (length ps) (length ss))
                        else typecheckArgs (map snd ps) ss
      (t'@(VarVarType _), t) -> addError (ComTypeError $ UnexpectedType (Var p n []) t' t)
      (_, t) -> addError (ComTypeError $ UnexpectedType (Var p f []) (ComVarType Unknown) (ComVarType Unknown))

  -- Resume / Unresume
  typecheckVarStmt (Unresume p n as) = typecheckVarStmt (Resume p n as)
  typecheckVarStmt (Resume p n as) = typeof (Var p n []) >>= \case
    (VarVarType (CoroutineT tn)) -> mapM (typeof . flip (Var p) []) as >>= \case
      ta | ta == tn  -> return ()
         | otherwise -> addError (ComTypeError $ UnexpectedType (Var p n []) (VarVarType (CoroutineT tn)) (VarVarType (CoroutineT ta)))
    t -> addError (ComTypeError $ UnexpectedType (Var p n []) (VarVarType $ CoroutineT []) t)
    
  typecheckVarStmt _ = return ()

  typecheckVarBinOp _ _ _ = return (ComVarType Unknown)
  typecheckVarUnOp _ _    = return (ComVarType Unknown)

  lookupProc nm = Reader.asks (Map.lookup nm . procedures)
  typeofVarProc c = return . VarVarType . CoroutineT $ map snd (coroparams c)

  typecheckVarValue (CoroutineVal _ _ ps) = return . VarVarType $ CoroutineT ps

-- Helpers

getstaticparams :: (VariantTypeChecker Coroutine) => String -> TypeCheckState Coroutine (Maybe (Params Coroutine))
getstaticparams m = do
  p <- Reader.ask
  case Map.lookup m (procedures p) of
    Nothing                -> return Nothing
    Just (VarProcedure c)  -> return . Just $ corostaticparams c
    Just _                 -> return Nothing
