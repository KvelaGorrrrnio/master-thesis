{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Coroutine.Stackless.Interpreter (module Coroutine.Stackless.Interpreter, module Common.Classes) where

import Common.Classes
import Common.Interpreter
import Coroutine.Stackless.Types
import Coroutine.Stackless.Error
import qualified Coroutine.Stackless.Static as Static
import qualified Coroutine.Stackless.Inverter as Inverter
import qualified Control.Monad.State as State
import qualified Control.Monad.Except as Except
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Common.Zipper as Zipper

instance VariantInterpreter Coroutine where
  interpret prog = Except.runExcept (State.execStateT interpretProgram prog)

  interpretProgram = do
    (l,ls) <- getCurrentLayers
    nm <- State.gets (name . progbody)
    case l:ls of
      [] -> Except.throwError . ComInterpreterError $ NoMoreStatements nm
      [l]  | Zipper.endz l && nm == "" -> return ()
      [l]  | Zipper.endz l -> Except.throwError . ComInterpreterError $ NoMoreStatements nm
      l:ls | Zipper.endz l -> returnLayer >> interpretProgram
           | otherwise -> interpretStmt >> interpretProgram

  -- Spawn
  interpretVarStmt (Spawn pos l f q as) = do
    multiReferencingArgs pos (l:as)
    sym <- getStore
    fsym <- getProcedureTable
    case Map.lookup l sym of
      Just (ComValue Empty) ->
        -- Check that yield is present in the coroutine
        case Map.lookup f fsym of
          Just (VarProcedure c) -> do
            let bindings = Map.fromList $ zip (map fst (corostaticparams c)) (Maybe.mapMaybe (`Map.lookup` sym) as)
            let store' = Map.unionWith (\_ v -> v) (corostore c) bindings
            cid <- genID
            taued <- atTau $ c { corobody = map Zipper.begin (corobody c), corolabel = l, coroid = cid, corostore = store' }
            taued' <- buildLayers taued
            csym <- getCoroutines
            setCoroutines $ Map.insert cid (VarProcedure taued') csym
            setStore (Map.insert l (VarValue $ CoroutineVal f cid (map snd $ coroparams taued')) sym)
            nextStmt
          _ -> Except.throwError . VarInterpreterError $ UnknownCoroutine f
      Just v -> Except.throwError . ComInterpreterError $ NonEmpty (Var pos l []) v
      Nothing -> Except.throwError . ComInterpreterError . UnknownVariable $ Var (-1,-1) l []
    where atTau :: VarProcedure Coroutine -> InterpreterState Coroutine (VarProcedure Coroutine)
          atTau c = case corobody c of
            [] -> Except.throwError . VarInterpreterError $ BreakpointNotFound q
            l:ls -> case Zipper.cursor l of
              Just (VarStmt (Yield _ q')) | q == q' -> return c
              Just (ComStmt (If pos e1 s1 s2 e2 b)) ->
                case (atTau' s1, atTau' s2) of
                  (Just s1', _) -> return $ c { corobody = Zipper.replace (ComStmt $ If pos e1 s1' s2 e2 True) l:ls }
                  (_, Just s2') -> return $ c { corobody = Zipper.replace (ComStmt $ If pos e1 s1 s2' e2 False) l:ls }
                  _ -> atTau (c { corobody = Zipper.right (Zipper.replace (ComStmt $ If pos e1 (Zipper.end s1) (Zipper.end s2) e2 b) l):ls })
              Just (ComStmt (Loop pos e1 s1 s2 e2 b)) ->
                case (atTau' s1, atTau' s2) of
                  (Just s1', _) -> return $ c { corobody = Zipper.replace (ComStmt $ Loop pos e1 s1' s2 e2 True) l:ls }
                  (_, Just s2') -> return $ c { corobody = Zipper.replace (ComStmt $ Loop pos e1 s1 s2' e2 False) l:ls }
                  _ -> atTau (c { corobody = Zipper.right (Zipper.replace (ComStmt $ Loop pos e1 (Zipper.end s1) (Zipper.end s2) e2 b) l):ls })
              Just _ -> atTau (c { corobody = Zipper.right l:ls })
              Nothing -> Except.throwError . VarInterpreterError $ BreakpointNotFound q
          atTau' :: Stmts Coroutine -> Maybe (Stmts Coroutine)
          atTau' z = case Zipper.cursor z of
              Just (VarStmt (Yield _ q')) | q == q' -> Just z
              Just (ComStmt (If pos e1 s1 s2 e2 b)) ->
                case (atTau' s1, atTau' s2) of
                  (Just s1', _) -> Just (Zipper.replace (ComStmt $ If pos e1 s1' s2 e2 True) z)
                  (_, Just s2') -> Just (Zipper.replace (ComStmt $ If pos e1 s1 s2' e2 False) z)
                  _ -> atTau' (Zipper.right (Zipper.replace (ComStmt $ If pos e1 (Zipper.end s1) (Zipper.end s2) e2 b) z))
              Just (ComStmt (Loop pos e1 s1 s2 e2 b)) ->
                case (atTau' s1, atTau' s2) of
                  (Just s1', _) -> Just (Zipper.replace (ComStmt $ Loop pos e1 s1' s2 e2 True) z)
                  (_, Just s2') -> Just (Zipper.replace (ComStmt $ Loop pos e1 s1 s2' e2 False) z)
                  _ -> atTau' (Zipper.right (Zipper.replace (ComStmt $ Loop pos e1 (Zipper.end s1) (Zipper.end s2) e2 b) z))
              Just _ -> atTau' (Zipper.right z)
              Nothing -> Nothing
          buildLayers :: VarProcedure Coroutine -> InterpreterState Coroutine (VarProcedure Coroutine)
          buildLayers c = do
            (l,ls) <- getLayers (VarProcedure c)
            case Zipper.cursor l of
              Just (ComStmt (If pos e1 s1 s2 e2 True)) -> setLayers (VarProcedure c) (s1:l:ls) >>= \case
                (VarProcedure c') -> buildLayers c'
              Just (ComStmt (If pos e1 s1 s2 e2 False)) -> setLayers (VarProcedure c) (s2:l:ls) >>= \case
                (VarProcedure c') -> buildLayers c'
              Just (ComStmt (Loop pos e1 s1 s2 e2 True)) -> setLayers (VarProcedure c) (s1:l:ls) >>= \case
                (VarProcedure c') -> buildLayers c'
              Just (ComStmt (Loop pos e1 s1 s2 e2 False)) -> setLayers (VarProcedure c) (s2:l:ls) >>= \case
                (VarProcedure c') -> buildLayers c'
              _ -> return c

  -- Despawn
  interpretVarStmt (Despawn pos l f q as) = do
    multiReferencingArgs pos (l:as)
    sym <- getStore
    csym <- getCoroutines
    case Map.lookup l sym of
      Just (VarValue (CoroutineVal cnm cid cps)) ->
        case Map.lookup cid csym of
          Just (VarProcedure c)
            | coroname c /= f -> Except.throwError . VarInterpreterError $ UnexpectedCoroutineName f (coroname c)
            | otherwise -> do
            (layer,layers) <-  getLayers (VarProcedure c)
            -- Check if at yield
            case Zipper.cursor layer of
              Just (VarStmt (Yield _ q')) | q == q' -> do
                let args = Map.fromList $ zip (map fst (corostaticparams c)) (Maybe.mapMaybe (`Map.lookup` sym) as)
                let bindings = Map.fromList $ zip (map fst (corostaticparams c)) (Maybe.mapMaybe ((`Map.lookup` corostore c) . fst) (corostaticparams c))
                let store' = Map.difference (corostore c) bindings
                let nonempty = Map.filter (ComValue Empty /=) store'
                if not (null nonempty)
                    then Except.throwError . ComInterpreterError $ NonEmptyLocalStore l nonempty
                    else if Map.elems args /= Map.elems bindings
                          then Except.throwError (ComInterpreterError (InconsistentArgs l args bindings))
                          else do
                            elv <- emptyValue (Var pos l [])
                            setStore      $ Map.insert l elv sym
                            setCoroutines $ Map.delete cid csym
                            nextStmt
              Just (VarStmt (Yield _ q')) -> Except.throwError . VarInterpreterError $ UnexpectedBreakpoint q q'
              _ -> Except.throwError . VarInterpreterError $ NotAtBreakpoint pos
          Just vf -> Except.throwError . VarInterpreterError $ ExpectedCoroutine vf
          Nothing -> Except.throwError . ComInterpreterError . UnknownVariable $ Var pos l []
      Just tauv -> do
        p <- store <$> getEnv
        undefined `debug` ((show tauv ++ show pos) ++ show p) -- TODO: fails
      Nothing -> Except.throwError . ComInterpreterError $ UnknownVariable (Var pos l [])

  -- Resume
  interpretVarStmt (Resume pos l as) = do
    multiReferencingArgs pos (l:as)
    sym <- getStore
    csym <- getCoroutines
    case Map.lookup l sym of
      Just (VarValue (CoroutineVal cnm cid cps)) ->
        case Map.lookup cid csym of
          Just (VarProcedure c) -> do
            layer <- fst <$> getLayers (VarProcedure c)
            case Zipper.cursor layer of
              (Just (VarStmt (Yield _ _))) -> do
                elv <- emptyValue (Var pos l [])
                setStore (Map.insert l elv sym)
                -- Change environment
                pbody <- getEnv
                setEnv $ VarProcedure c
                case pbody of
                  VarProcedure pbody' -> do
                    pcid <- getCoroutineId pbody
                    getStack >>= setStack . (genStackId pcid:)
                    setCoroutines $ Map.insert pcid pbody csym
                  ComProcedure _ -> getStack >>= setStack . (pbody:)
                nextStmt
                -- Bind arguments
                let args = Map.fromList $ zip (map fst (coroparams c)) (Maybe.mapMaybe (`Map.lookup` sym) as)
                let sym' = Map.unionWith (\_ v -> v) (corostore c) args
                setStore sym'
              _ -> Except.throwError . VarInterpreterError $ NotAtBreakpoint pos
          Just vf -> Except.throwError . VarInterpreterError $ ExpectedCoroutine vf
          Nothing -> Except.throwError . ComInterpreterError . UnknownVariable $ Var pos l []
      Just tauv -> undefined `debug` (show tauv ++ show pos) -- TODO: fails
      Nothing -> Except.throwError . ComInterpreterError $ UnknownVariable (Var pos l [])

  -- Unresume
  interpretVarStmt (Unresume pos l as) = do
    multiReferencingArgs pos (l:as)
    sym <- getStore
    csym <- getCoroutines
    case Map.lookup l sym of
      Just (VarValue (CoroutineVal cnm cid cps)) ->
        case Map.lookup cid csym of
          Just (VarProcedure c) -> do
            layer <- fst <$> getLayers (VarProcedure c)
            case Zipper.cursor layer of
              Just (VarStmt (Yield _ _)) -> do
                elv <- emptyValue (Var pos l [])
                setStore (Map.insert l elv sym)
                --------
                --p <- State.get
                --pcid <- getCoroutineId $ progbody p
                --State.put (p { progbody = VarProcedure c, progstack = (genStackId pcid):progstack p })
                --setCoroutines $ Map.insert pcid (progbody p) csym 
                --nextStmt
                -- Change environment
                pbody <- getEnv
                case pbody of
                  VarProcedure pbody' -> do
                    pcid <- getCoroutineId pbody
                    getStack >>= setStack . (genStackId pcid:)
                    setCoroutines $ Map.insert pcid pbody csym
                  ComProcedure _ -> getStack >>= setStack . (pbody:)
                setEnv . VarProcedure $ Inverter.inverseVarProcedure c
                -- Bind arguments
                let args = Map.fromList $ zip (map fst (coroparams c)) (Maybe.mapMaybe (`Map.lookup` sym) as)
                let sym' = Map.unionWith (\_ v -> v) (corostore c) args
                setStore sym'
              x -> Except.throwError . VarInterpreterError $ NotAtBreakpoint pos
          Just vf -> Except.throwError . VarInterpreterError $ ExpectedCoroutine vf
          Nothing -> Except.throwError . ComInterpreterError . UnknownVariable $ Var pos l []
      Just tauv -> undefined `debug` (show tauv ++ show pos) -- TODO: fails
      Nothing -> Except.throwError . ComInterpreterError $ UnknownVariable (Var pos l [])


  -- Yield
  interpretVarStmt (Yield pos _) = do
    p <- State.get
    csym <- getCoroutines
    case progbody p of
      (VarProcedure c) -> do
        (c',stack) <- case progstack p of
                        [] -> Except.throwError . ComInterpreterError $ EmptyStack
                        (VarProcedure (Coroutine { coroid = cid })):stack ->
                          case Map.lookup cid csym of
                            Just c' -> return (c',stack)
                            Nothing -> Except.throwError . VarInterpreterError $ UnknownCoroutineId pos cid
                        c'@(ComProcedure _):stack -> return (c',stack)
        layer <- fst <$> getLayers c'
        case Zipper.cursor layer of
          Just (VarStmt (Resume _ l as)) -> do
              -- Compute new local store
              let args = Map.fromList $ zip (map fst $ coroparams c) (map (const (ComValue Empty) . fst) (coroparams c))
              let localsym = Map.unionWith (\_ v -> v) (corostore c) args
              -- Change environment
              State.put $ p { progstack = stack, progbody = c' }
              sym <- getStore
              setCoroutines $ Map.insert (coroid c) (VarProcedure (c { corostore = localsym })) csym
              setStore $ Map.insert l (VarValue $ CoroutineVal (coroname c) (coroid c) (map snd $ coroparams c)) sym
              -- Unbind arguments
              sym' <- getStore
              let rets = Map.fromList $ zip as (Maybe.mapMaybe ((`Map.lookup` corostore c) . fst) (coroparams c))
              let localsym' = Map.unionWith (\_ v -> v) sym' rets
              setStore localsym'
              -- Move on
              nextStmt
          Just (VarStmt (Unresume _ l as)) -> do
            nextStmt
            State.gets progbody >>= \case
              (VarProcedure c) -> do
                -- Compute new local store
                let args = Map.fromList $ zip (map fst $ coroparams c) (map (const (ComValue Empty) . fst) (coroparams c))
                let localsym = Map.unionWith (\_ v -> v) (corostore c) args
                -- Change environment
                State.put $ p { progstack = stack, progbody = c' }
                sym <- getStore
                setCoroutines $ Map.insert (coroid c) (VarProcedure . Inverter.inverseVarProcedure $ c { corostore = localsym }) csym
                setStore $ Map.insert l (VarValue $ CoroutineVal (coroname c) (coroid c) (map snd $ coroparams c)) sym
                -- Unbind arguments
                sym' <- getStore
                let rets = Map.fromList $ zip as (Maybe.mapMaybe ((`Map.lookup` corostore c) . fst) (coroparams c))
                let localsym' = Map.unionWith (\_ v -> v) sym' rets
                setStore localsym'
                -- Move on
                nextStmt
          s -> Except.throwError . VarInterpreterError $ NotAtBreakpoint pos
      _ -> Except.throwError . VarInterpreterError $ YieldedFromNonCoroutine pos

  returnVarLayer s l = Except.throwError . ComInterpreterError $ NonLayeredStmt (VarStmt s)

  selfReferencingVarValue _ _ = return ()

  emptyValue n = do
    tsym <- types <$> getEnv
    case Map.lookup (varbase n) tsym of
      Nothing -> return $ ComValue Empty
      Just (ComVarType Unknown) -> return $ ComValue Empty
      Just t -> return . Static.type2symbol . Maybe.fromMaybe (ComVarType Unknown) $ Static.getIndexedType t (varindices n)

  -- Evaluate
  evalVarBinOp _ v _ _ = return v
  evalVarUnOp _ v = return v
  evalVarValue v = return $ VarValue v
  
  -- Utilities
  addLayer l = do
    (l',ls) <- getCurrentLayers
    setCurrentLayers (l:l':ls)

  getLayers (VarProcedure c) = 
    case corobody c of
      [] -> Except.throwError . ComInterpreterError $ NoLayers
      l:ls -> return (l,ls)
  getLayers (ComProcedure f) = 
    case funcbody f of
      [] -> Except.throwError . ComInterpreterError $ NoLayers
      l:ls -> return (l,ls)

  getCurrentLayers = State.gets progbody >>= getLayers

  setLayers (VarProcedure c) ls =
    return . VarProcedure $ c { corobody = ls }
  setLayers (ComProcedure f) ls =
    return . ComProcedure $ f { funcbody = ls }

  setCurrentLayers ls = do
    prog <- State.get
    p <- setLayers (progbody prog) ls
    State.put $ prog { progbody = p }

  getEnv = State.gets progbody
  setEnv progbody' = do
    prog <- State.get
    State.put $ prog { progbody = progbody' }

  getStack = State.gets progstack
  setStack stack' = do
    prog <- State.get
    State.put $ prog { progstack = stack' }

  getStore = State.gets (store . progbody)

  setStore s = do
    prog <- State.get
    case progbody prog of
      (VarProcedure c) -> State.put $ prog { progbody = VarProcedure $ c { corostore = s } }
      (ComProcedure f) -> State.put $ prog { progbody = ComProcedure $ f { funcstore = s } }

  getStmt = Zipper.cursor . fst <$> (State.gets progbody >>= getLayers)

  getProcedureTable = State.gets procedures

  nextStmt = do
    prog <- State.get
    (layer,ls) <- getLayers (progbody prog)
    case progbody prog of
      (VarProcedure c) -> State.put $ prog { progbody = VarProcedure $ c { corobody = Zipper.right layer:ls }  }
      (ComProcedure f) -> State.put $ prog { progbody = ComProcedure $ f { funcbody = Zipper.right layer:ls }  }

  prevStmt = do
    prog <- State.get
    (layer,ls) <- getLayers (progbody prog)
    case progbody prog of
      (VarProcedure c) -> State.put $ prog { progbody = VarProcedure $ c { corobody = Zipper.left layer:ls }  }
      (ComProcedure f) -> State.put $ prog { progbody = ComProcedure $ f { funcbody = Zipper.left layer:ls }  }

  insertStmt s = do
    prog <- State.get
    (layer,ls) <- getLayers (progbody prog)
    case progbody prog of
      (VarProcedure c) -> State.put $ prog { progbody = VarProcedure $ c { corobody = Zipper.insert s layer:ls }  }
      (ComProcedure f) -> State.put $ prog { progbody = ComProcedure $ f { funcbody = Zipper.insert s layer:ls }  }

  removeStmt = do
    prog <- State.get
    (layer,ls) <- getLayers (progbody prog)
    case progbody prog of
      (VarProcedure c) ->
        case Zipper.cursor layer of
          Nothing -> return Nothing
          s -> State.put (prog { progbody = VarProcedure $ c { corobody = Zipper.delete layer:ls }}) >> return s
      (ComProcedure f) ->
        case Zipper.cursor layer of
          Nothing -> return Nothing
          s -> State.put (prog { progbody = ComProcedure $ f { funcbody = Zipper.delete layer:ls }}) >> return s

-- Helpers
getCoroutines :: InterpreterState Coroutine (Map.Map CoroutineId (Procedure Coroutine))
getCoroutines = State.gets coroutines

setCoroutines :: Map.Map CoroutineId (Procedure Coroutine) -> InterpreterState Coroutine ()
setCoroutines coroutines = do
  prog <- State.get
  State.put $ prog { coroutines = coroutines }

getCoroutineId :: Procedure Coroutine -> InterpreterState Coroutine CoroutineId
getCoroutineId c@(ComProcedure _) = Except.throwError . VarInterpreterError $ ExpectedCoroutine c
getCoroutineId (VarProcedure c)   = return $ coroid c

genID :: InterpreterState Coroutine CoroutineId
genID = State.gets (flip genID' 0 . Map.keys . coroutines)
  where genID' keys n
          | n `elem` keys = genID' keys (n + 1)
          | otherwise     = n

genStackId :: CoroutineId -> Procedure Coroutine
genStackId cid = VarProcedure $ Coroutine "" cid "" [] [] [] Map.empty Map.empty
