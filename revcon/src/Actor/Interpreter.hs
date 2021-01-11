{-# LANGUAGE FlexibleContexts, LambdaCase, TupleSections #-}
module Actor.Interpreter (module Actor.Interpreter, module Common.Classes) where

import Common.Classes
import Common.Interpreter

import Actor.Types
import Actor.Error
import qualified Actor.Static as Static
import qualified Actor.Inverter as Inverter

import qualified Common.Queue as Queue
import qualified Common.Zipper as Zipper
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Control.Monad as Monad
import qualified Control.Monad.State as State
import qualified Control.Monad.Except as Except

instance VariantInterpreter Actor where
  interpret prog = Except.runExcept (State.execStateT interpretProgram prog)

  interpretProgram = do
    (l,ls) <- getCurrentLayers
    nm <- State.gets (name . progbody)
    case l:ls of
      [] -> Except.throwError . ComInterpreterError $ NoMoreStatements nm
      [l]  | Zipper.endz l && nm == "" -> return ()
      [l]  | Zipper.endz l -> returnFromMailpoint >> interpretProgram
      l:ls | Zipper.endz l -> returnLayer >> interpretProgram
           | otherwise -> interpretStmt >> interpretProgram

  -- Spawn
  interpretVarStmt (Spawn pos l f) = do -- TODO: remove as from Spawn
    sym <- getStore
    fsym <- getProcedureTable
    case Map.lookup l sym of
      Just (ComValue Empty) -> do
        case Map.lookup f fsym of
          Just (VarProcedure a) -> do
            tau <- genID
            setStore (Map.insert l (VarValue . ActorVal $ tau) sym)
            asym <- getActors
            globstore <- State.gets globalstore
            let syma = if f /= "main" then actostore a
                                      else Map.union globstore (actostore a)
            setActors $ Map.insert tau (a { actorid = tau, actostore = syma }) asym
            nextStmt
          Just fv -> undefined
          Nothing -> undefined
      Just v -> Except.throwError . ComInterpreterError $ NonEmpty (Var pos l []) v
      Nothing -> Except.throwError . ComInterpreterError $ UnknownVariable (Var pos l [])

  -- Despawn
  interpretVarStmt (Despawn pos l f) = do -- TODO: remove arguments from despawn
    sym <- getStore
    asym <- getActors
    case Map.lookup l sym of
      Just (VarValue (ActorVal tau)) ->
        case Map.lookup tau asym of
          Just a
            | actoname a /= f -> undefined
            | otherwise -> do
              globalstore <- State.gets globalstore
              let nonempty = if f /= "main" then Map.empty else Map.filter (ComValue Empty /=) (Map.difference (actostore a) globalstore)
              if not (null nonempty)
                 then Except.throwError . ComInterpreterError $ NonEmptyLocalStore l nonempty
                 else do
                   elv <- emptyValue (Var pos l [])
                   setStore  $ Map.insert l elv sym
                   setActors $ Map.delete tau asym
                   nextStmt
          Nothing -> undefined
      Just tauv -> undefined `debug` (show tauv ++ show pos) -- TODO: fails
      Nothing -> Except.throwError . ComInterpreterError $ UnknownVariable (Var pos l [])

  -- Send
  interpretVarStmt (Send pos (Right _) mp as) = do
    multiReferencingArgs pos as
    sym <- getStore
    self <- getActorid
    asv <- mapM (eval . Value . ComValue . RefVal . flip (Var pos) []) as
    easv <- mapM (emptyValue . flip (Var pos) []) as
    setStore $ Map.unionWith (\_ v->v) sym $ Map.fromList (zip as easv)
    sendMessage self self mp asv
    nextStmt

  interpretVarStmt (Send pos (Left l) mp as) = do
    multiReferencingArgs pos (l:as)
    self <- getActorid
    sym <- getStore
    asv <- mapM (eval . Value . ComValue . RefVal . flip (Var pos) []) as
    case Map.lookup l sym of
      Just (VarValue (ActorVal tau)) -> do
        easv <- mapM (emptyValue . flip (Var pos) []) as
        setStore $ Map.unionWith (\_ v->v) sym $ Map.fromList (zip as easv)
        sendMessage self tau mp asv
        nextStmt
      _ -> Except.throwError . ComInterpreterError $ UnknownVariable (Var pos l [])

  -- Unsend
  interpretVarStmt (Unsend pos l mp as) = do
    sym <- getStore
    tau <- case l of
             Right _ -> getActorid
             Left l' -> case Map.lookup l' sym of
                          Just (VarValue (ActorVal tau)) -> return tau
                          Nothing -> Except.throwError . ComInterpreterError $ UnknownVariable (Var pos l' [])

    self <- getActorid
    asv <- unsendMessage self tau mp
    setStore $ Map.unionWith (\_ v->v) sym $ Map.fromList $ zip as asv
    nextStmt

  -- Deliver
  interpretVarStmt (Deliver pos m) = do
    sym <- getStore
    asym <- getActors
    case Map.lookup m sym of
      Just (VarValue (MessageVal from to mp asv)) -> do
        case Map.lookup to asym of
          Just a ->
            case Map.lookup mp (mailpoints a) of
              Just mpv -> do
                let senderEntry = case mailpointSender mpv of
                                    Nothing -> []
                                    Just sender -> [(sender,VarValue $ ActorVal from)]
                    newsym = Map.unionWith (\_ v->v) (actostore a) (Map.fromList $ senderEntry ++ zip (map fst $ mailpointParams mpv) asv)
                nextStmt
                scheduler <- getEnv
                setEnv . VarProcedure $ a { mailpoint = mailpointName mpv, actobody = [mailpointBody mpv], actostore = newsym }
                getStack >>= setStack . (scheduler:)
              Nothing -> Except.throwError . VarInterpreterError $ UnknownMailpoint pos (actoname a) mp
          Nothing -> Except.throwError . VarInterpreterError $ UnknownActorId to
      _ -> Except.throwError . ComInterpreterError . UnknownVariable $ Var pos m []

  -- Undeliver
  interpretVarStmt (Undeliver pos m) = do
    sym <- getStore
    asym <- getActors
    case Map.lookup m sym of
      Just (VarValue (MessageVal from to mp asv)) -> do
        case Map.lookup to asym of
          Just a ->
            case Map.lookup mp (mailpoints a) of
              Just mpv -> do
                let senderEntry = case mailpointSender mpv of
                                    Nothing -> []
                                    Just sender -> [(sender,VarValue $ ActorVal from)]
                    newsym = Map.unionWith (\_ v->v) (actostore a) (Map.fromList $ senderEntry ++ zip (map fst $ mailpointParams mpv) asv)
                nextStmt
                scheduler <- getEnv
                setEnv . Inverter.inverseProcedure . VarProcedure $ a { mailpoint = mailpointName mpv, actobody = [Zipper.end $ mailpointBody mpv], actostore = newsym }
                getStack >>= setStack . (scheduler:)
              Nothing -> Except.throwError . VarInterpreterError $ UnknownMailpoint pos (actoname a) mp
          Nothing -> do
            prog <- State.get
            Except.throwError . VarInterpreterError $ UnknownActorId to
      _ -> Except.throwError . ComInterpreterError . UnknownVariable $ Var pos m []

  --interpretVarStmt s = nextStmt `debug` ("skipping " ++ show s)

  evalVarBinOp _ v _ _ = return v
  evalVarUnOp _ v = return v
  evalVarValue v = return $ VarValue v

  returnVarLayer s l = Except.throwError . ComInterpreterError $ NonLayeredStmt (VarStmt s)

  selfReferencingVarValue _ _ = return ()
  emptyValue n = do
    tsym <- types <$> getEnv
    case Map.lookup (varbase n) tsym of
      Nothing -> return $ ComValue Empty
      Just (ComVarType Unknown) -> return $ ComValue Empty
      Just t -> return . Static.type2symbol . Maybe.fromMaybe (ComVarType Unknown) $ Static.getIndexedType t (varindices n)

  -- Layers
  addLayer l = do
    (l',ls) <- getCurrentLayers
    setCurrentLayers (l:l':ls)

  getLayers (VarProcedure a) = 
    case actobody a of
      [] -> Except.throwError . ComInterpreterError $ NoLayers
      l:ls -> return (l,ls)
  getLayers (ComProcedure f) = 
    case funcbody f of
      [] -> Except.throwError . ComInterpreterError $ NoLayers
      l:ls -> return (l,ls)
  getCurrentLayers = State.gets progbody >>= getLayers

  setLayers (VarProcedure a) ls =
    return . VarProcedure $ a { actobody = ls }
  setLayers (ComProcedure f) ls =
    return . ComProcedure $ f { funcbody = ls }

  setCurrentLayers ls = do
    prog <- State.get
    p <- setLayers (progbody prog) ls
    State.put $ prog { progbody = p }

  -- Stack
  getEnv = State.gets progbody
  setEnv progbody' = do
    prog <- State.get
    State.put $ prog { progbody = progbody' }

  getStack = State.gets progstack
  setStack stack' = do
    prog <- State.get
    State.put $ prog { progstack = stack' }

  -- Utilities
  getStore = State.gets (store . progbody)
  setStore sym = do
    prog <- State.get
    case progbody prog of
      (VarProcedure c) -> State.put $ prog { progbody = VarProcedure $ c { actostore = sym } }
      (ComProcedure f) -> State.put $ prog { progbody = ComProcedure $ f { funcstore = sym } }

  getProcedureTable = State.gets procedures
  getStmt           = Zipper.cursor . fst <$> (State.gets progbody >>= getLayers)

  nextStmt = do
    prog <- State.get
    (layer,ls) <- getLayers (progbody prog)
    case progbody prog of
      (VarProcedure c) -> State.put $ prog { progbody = VarProcedure $ c { actobody = Zipper.right layer:ls }  }
      (ComProcedure f) -> State.put $ prog { progbody = ComProcedure $ f { funcbody = Zipper.right layer:ls }  }

  prevStmt = do
    prog <- State.get
    (layer,ls) <- getLayers (progbody prog)
    case progbody prog of
      (VarProcedure c) -> State.put $ prog { progbody = VarProcedure $ c { actobody = Zipper.left layer:ls }  }
      (ComProcedure f) -> State.put $ prog { progbody = ComProcedure $ f { funcbody = Zipper.left layer:ls }  }

  insertStmt s = do
    prog <- State.get
    (layer,ls) <- getLayers (progbody prog)
    case progbody prog of
      (VarProcedure c) -> State.put $ prog { progbody = VarProcedure $ c { actobody = Zipper.insert s layer:ls }  }
      (ComProcedure f) -> State.put $ prog { progbody = ComProcedure $ f { funcbody = Zipper.insert s layer:ls }  }

  removeStmt = do
    prog <- State.get
    (layer,ls) <- getLayers (progbody prog)
    case progbody prog of
      (VarProcedure c) ->
        case Zipper.cursor layer of
          Nothing -> return Nothing
          s -> State.put (prog { progbody = VarProcedure $ c { actobody = Zipper.delete layer:ls }}) >> return s
      (ComProcedure f) ->
        case Zipper.cursor layer of
          Nothing -> return Nothing
          s -> State.put (prog { progbody = ComProcedure $ f { funcbody = Zipper.delete layer:ls }}) >> return s

-- Helpers
getActors :: InterpreterState Actor (Map.Map ActorId (VarProcedure Actor))
getActors = State.gets actors

setActors :: Map.Map ActorId (VarProcedure Actor) -> InterpreterState Actor ()
setActors actors = do
  prog <- State.get
  State.put $ prog { actors = actors }

getActorid :: InterpreterState Actor ActorId
getActorid = getEnv >>= \case
  VarProcedure self -> return $ actorid self

sendMessage :: ActorId -> ActorId -> String -> [Value Actor] -> InterpreterState Actor ()
sendMessage from to mp as = do
  stack <- getStack
  if null stack then sendMessageScheduler from to mp as
                else sendMessageActor stack from to mp as

sendMessageActor :: [Procedure Actor] -> ActorId -> ActorId -> String -> [Value Actor] -> InterpreterState Actor ()
sendMessageActor stack from to mp as = 
  case last stack of
    VarProcedure scheduler -> do
      let message = VarValue (MessageVal from to mp as)
      let sym = actostore scheduler
      case Map.lookup "!mailbox" sym of
        Just (ComValue (QueueVal vq)) -> do
          let sym' = Map.insert "!mailbox" (ComValue . QueueVal $ Queue.enqueue message vq) sym
          setStack $ init stack ++ [VarProcedure $ scheduler { actostore = sym' }]
        Just vq -> Except.throwError . ComInterpreterError $ NonQueueValue (Var (-1,-1) "!mailbox" []) vq
        Nothing -> Except.throwError . ComInterpreterError . UnknownVariable $ Var (-1,-1) "!mailbox" []

sendMessageScheduler :: ActorId -> ActorId -> String -> [Value Actor] -> InterpreterState Actor ()
sendMessageScheduler from to mp as = getEnv >>= \case
  VarProcedure scheduler -> do
    let message = VarValue (MessageVal from to mp as)
    let sym = actostore scheduler
    case Map.lookup "!mailbox" sym of
      Just (ComValue (QueueVal qv)) -> setStore $ Map.insert "!mailbox" (ComValue . QueueVal $ Queue.enqueue message qv) sym
      Just qv -> Except.throwError . ComInterpreterError $ NonQueueValue (Var (-1,-1) "!mailbox" []) qv
      Nothing -> Except.throwError . ComInterpreterError . UnknownVariable $ Var (-1,-1) "!mailbox" []

unsendMessage :: ActorId -> ActorId -> String -> InterpreterState Actor [Value Actor]
unsendMessage from to mp = do
  stack <- getStack
  if null stack then unsendMessageScheduler from to mp
                else unsendMessageActor stack from to mp

unsendMessageActor :: [Procedure Actor] -> ActorId -> ActorId -> String -> InterpreterState Actor [Value Actor]
unsendMessageActor stack from to mp = 
  case last stack of
    VarProcedure scheduler -> do
      let sym = actostore scheduler
      case Map.lookup "!mailbox" sym of
        Just (ComValue (QueueVal qv)) -> do
          case Queue.unenqueue qv of
            (Just (VarValue (MessageVal from' to' mp' asv)), qv')
              | from == from' && to == to' && mp == mp' -> do
                let sym' = Map.insert "!mailbox" (ComValue . QueueVal $ qv') sym
                setStack $ init stack ++ [VarProcedure $ scheduler { actostore = sym' }]
                return asv
              | otherwise -> Except.throwError . VarInterpreterError $ WrongMessage (from,to,mp) (from',to',mp')
            (_, qv') -> Except.throwError . VarInterpreterError $ NoMessages
        Just qv -> Except.throwError . ComInterpreterError $ NonQueueValue (Var (-1,-1) "!mailbox" []) qv
        Nothing -> Except.throwError . ComInterpreterError . UnknownVariable $ Var (-1,-1) "!mailbox" []

unsendMessageScheduler :: ActorId -> ActorId -> String -> InterpreterState Actor [Value Actor]
unsendMessageScheduler from to mp = getEnv >>= \case
  VarProcedure scheduler -> do
    let sym = actostore scheduler
    case Map.lookup "!mailbox" sym of
      Just (ComValue (QueueVal qv)) -> do
        case Queue.unenqueue qv of
          (Just (VarValue (MessageVal from' to' mp' asv)), qv')
            | from == from' && to == to' && mp == mp' -> do
              setStore $ Map.insert "!mailbox" (ComValue . QueueVal $ qv') sym
              return asv
            | otherwise -> Except.throwError . VarInterpreterError $ WrongMessage (from,to,mp) (from',to',mp')
          (_, qv') -> Except.throwError . VarInterpreterError $ NoMessages
      Just qv -> Except.throwError . ComInterpreterError $ NonQueueValue (Var (-1,-1) "!mailbox" []) qv
      Nothing -> Except.throwError . ComInterpreterError . UnknownVariable $ Var (-1,-1) "!mailbox" []

returnFromMailpoint :: InterpreterState Actor ()
returnFromMailpoint = getStack >>= \case
  [scheduler] -> getEnv >>= \case
    VarProcedure me -> do
      -- Clean up in actor mailpoint and store
      let mpv = Maybe.fromJust $ Map.lookup (mailpoint me) (mailpoints me)
      asv <- mapM (eval . Value . ComValue . RefVal . flip (Var (-1,-1)) [] . fst) (mailpointParams mpv)
      let senderEntry = case mailpointSender mpv of
                          Nothing -> []
                          Just sender -> [(sender,VarVarType ActorT)]
      --clearedEntries <- zip (map fst $ senderEntry ++ mailpointParams mpv) <$> mapM (emptyValue . flip (Var (-1,-1)) [] . fst) (senderEntry ++ mailpointParams mpv)
      let meSym = Map.unionWith (\_ v->v) (actostore me) (Map.fromList $ map (\(k,_)->(k,ComValue Empty)) (senderEntry ++ mailpointParams mpv))
      asym <- getActors
      setActors $ Map.insert (actorid me) (me { mailpoint = "", actobody = [], actostore = meSym }) asym
      -- Change to scheduler
      setStack []
      setEnv scheduler
      -- Store results in message
      sym <- getStore
      let VarValue (MessageVal from to mp _) = Maybe.fromJust (Map.lookup "!message" sym)
      setStore $ Map.insert "!message" (VarValue (MessageVal from to mp asv)) sym
      -- Store globalstore
      Monad.when (actoname me == "main") $ do
        prog <- State.get
        State.put prog { globalstore = Map.intersection meSym (globalstore prog) }
    _ -> undefined
  x -> do 
    p <- State.get
    undefined `debug` show p

genID :: InterpreterState Actor ActorId
genID = State.gets (flip genID' 0 . Map.keys . actors)
  where genID' keys n
          | n `elem` keys = genID' keys (n + 1)
          | otherwise     = n
