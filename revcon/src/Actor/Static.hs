{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Actor.Static (module Actor.Static, module Common.Static, module Common.Classes) where

import Actor.Types
import Actor.Error
import Common.Classes
import Common.Static

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Common.Zipper as Zipper
import qualified Common.Queue as Queue

import qualified Control.Monad.State as State
import qualified Control.Monad.Reader as Reader

instance TypedVariant Actor where
  getSymbols = store
  setSymbols (VarProcedure p) s = VarProcedure $ p { actostore = s }
  setSymbols (ComProcedure p) s = ComProcedure $ p { funcstore = s }
  settype n t m = Map.insert n t m
  gettype n m   = Map.lookup n m

  varVarType2symbol ActorT = ComValue Empty
  compareVarTypes ActorT (VarVarType ActorT) = Just $ VarVarType ActorT
  compareVarTypes _ _ = Nothing

instance VariantTypeChecker Actor where
  staticcheck p = settypes p >>= addGlobalTypesToMain >>= typecheck >>= initiateStores
    where addGlobalTypesToMain p = Right $ p { procedures = Map.map addGlobalTypes (procedures p) }
            where addGlobalTypes (VarProcedure a) | actoname a == "main" = VarProcedure $ a { actotypes = Map.union (actotypes a) (globaltypes p) }
                  addGlobalTypes c = c
          initiateStores p = Right $ (setStartup p) { procedures = Map.map initiateProcedureStore (procedures p), globalstore = Map.map type2symbol (globaltypes p) }
          initiateProcedureStore (VarProcedure a)
            | actoname a == "main" = VarProcedure $ a { actostore = Map.union (Map.map type2symbol (globaltypes p)) (Map.map (\_ -> ComValue Empty) (actotypes a)), actotypes = Map.union (globaltypes p) (actotypes a) }
            | otherwise            = VarProcedure $ a { actostore = Map.map (\_ -> ComValue Empty) (actotypes a) }
          initiateProcedureStore (ComProcedure p) = ComProcedure $ p { funcstore = Map.map (\_ -> ComValue Empty) (functypes p) }
          setStartup prog@Program{progbody = (VarProcedure a)} =
            prog { progbody = VarProcedure initialBody}
              where initialBody = a { actostore = initialStore (maininit main), actobody = [initialStmts (maininit main)] }
                    maininit (VarProcedure main) = Maybe.fromJust (Map.lookup "init" (mailpoints main))
                    main = Maybe.fromJust (Map.lookup "main" (procedures prog))
                    initialStore mp = Map.fromList [("main", ComValue Empty),("!mailbox", ComValue (QueueVal Queue.empty)),("!messages", ComValue (ListVal [])),("!message",ComValue Empty)]
                    initialStmts mp = Zipper.fromList [ VarStmt (Spawn (2,1) "main" "main")
                                                      , VarStmt (Send (3,1) (Left "main") "init" (map fst $ mailpointParams mp))
                                                      , ComStmt (Loop (4,1) loopCond (Zipper.fromList loopDo) Zipper.empty loopAss True)
                                                      , VarStmt (Despawn (7,1) "main" "main")
                                                      ]
                      where loopCond = UnOp (ComUnOp UnOpEmpty) (Value . ComValue . RefVal $ Var (4,4) "!messages" [])
                            loopAss  = UnOp (ComUnOp UnOpEmpty) (Value . ComValue . RefVal $ Var (6,4) "!mailbox" [])
                            loopDo   = [ ComStmt (Dequeue (-1,-1) (Var (-1,-1) "!message" []) (Var (-1,-1) "!mailbox" []))
                                       , VarStmt (Deliver (-1,-1) "!message")
                                       , ComStmt (Push (-1,-1) (Var (-1,-1) "!message" []) (Var (-1,-1) "!messages" []))
                                       ]

  settypes p = Right p { procedures = Map.map (`settypesProcedure` procedures p) (procedures p) }
  settypesVarProcedure a psym = a { actotypes = foldr1 Map.union (map settypesMailpoint (Map.elems $ mailpoints a)) }
    where settypesMailpoint mpv =
            let senderEntry = case mailpointSender mpv of
                                Nothing -> mailpointParams mpv
                                Just sender -> (sender,VarVarType ActorT):mailpointParams mpv in
                  fst $ State.execState (settypesLayers [mailpointBody mpv]) (Map.fromList (senderEntry ++ mailpointParams mpv), psym)

  -- Spawn
  settypesVarStmt (Spawn _ l f) = do
    (m,pt) <- State.get
    case gettype l m of
      Nothing ->
        case Map.lookup f pt of
          Just (VarProcedure _) -> State.put (settype l (VarVarType ActorT) m,pt)
          _ -> return ()
      Just _  -> return ()

  -- Despawn
  settypesVarStmt (Despawn _ l f) = do
    (m,pt) <- State.get
    case gettype l m of
      Nothing ->
        case Map.lookup f pt of
          Just (VarProcedure _) -> State.put (settype l (VarVarType $ ActorT) m,pt)
          _ -> return ()
      Just _  -> return ()

  -- rest
  settypesVarStmt _ = return ()

  -- Type check
  typecheck p = case Reader.runReader (State.execStateT (foldr ((>>) . typecheckProcedure) (return ()) (Map.elems $ procedures p)) ([],Map.empty)) p of
                   ([],_) -> Right p
                   (es,_) -> Left $ ColTypeError es

  -- Procedures (Actor)
  typecheckVarProcedure c = (foldr ((>>) . typecheckMailpoint) (return ()) (Map.elems $ mailpoints c) ) >> withTypeTable Map.empty
    where typecheckMailpoint mp = withTypeTable (actotypes c) >> typecheckLayers [mailpointBody mp]
          withTypeTable t = State.get >>= \(s,_) -> State.put (s,t)
          typecheckLayers ls = foldr ((>>) . typecheckStmts) (return ()) ls

  -- Spawn / Despawn
  typecheckVarStmt (Despawn p n f) = typecheckVarStmt (Spawn p n f)
  typecheckVarStmt s@(Spawn _ _ "main") = addError (ComTypeError $ IllegalProcedure "main" (VarStmt s))
  typecheckVarStmt (Spawn p n f) = do
    tn <- typeof (Var p n [])
    tf <- typeofProc f
    case (tf,tn) of
      (VarVarType ActorT, VarVarType ActorT) -> return ()
      (t'@(VarVarType _), t) -> addError . ComTypeError $ UnexpectedType (Var p n []) t' t
      (t,_)                  -> addError . ComTypeError $ UnexpectedType (Var p f []) (VarVarType ActorT) t

  -- Send / Desend
  typecheckVarStmt (Unsend p n m as) = typecheckVarStmt (Send p n m as)
  typecheckVarStmt (Send p (Right _) _ _) = return ()
  typecheckVarStmt (Send p (Left n) _ _) = typeof (Var p n []) >>= \case
    VarVarType ActorT -> return ()
    t -> addError . ComTypeError $ UnexpectedType (Var p n []) (VarVarType ActorT) t

  typecheckVarValue  = undefined
  typecheckVarBinOp  = undefined
  typecheckVarUnOp   = undefined

  lookupProc nm = Reader.asks (Map.lookup nm . procedures)
  typeofVarProc c = return $ VarVarType ActorT

