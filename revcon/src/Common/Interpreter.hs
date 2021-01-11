{-# LANGUAGE AllowAmbiguousTypes, PolyKinds, FlexibleContexts, UndecidableInstances, LambdaCase, TupleSections #-}
module Common.Interpreter (module Common.Interpreter) where

import Common.Types
import Common.Static
import Common.Classes
import Common.Error
import qualified Control.Monad as Monad (unless)
import qualified Control.Monad.State as State
import qualified Control.Monad.Except as Except (throwError)
import qualified Data.Map as Map
import qualified Common.Zipper as Zipper
import qualified Common.Queue as Queue
import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe
import qualified Common.Inverter as Inverter

-- Statements
interpretStmt :: (VariantInterpreter v) => InterpreterState v ()
interpretStmt = getStmt >>= \case
  (Just (ComStmt s)) -> interpretComStmt s
  (Just (VarStmt s)) -> interpretVarStmt s
  Nothing -> getEnv >>= Except.throwError . ComInterpreterError . NoMoreStatements . name

interpretComStmt :: (VariantInterpreter v) => ComStmt v -> InterpreterState v ()

-- Swap
interpretComStmt (Swap p n m) = do
  sym <- getStore
  case Map.lookup (varbase n) sym of
    Nothing -> Except.throwError . ComInterpreterError $ UnknownVariable n
    Just nv -> case Map.lookup (varbase m) sym of
                 Nothing -> Except.throwError . ComInterpreterError $ UnknownVariable m
                 Just mv -> do
                   nids <- mapM eval (varindices n)
                   mids <- mapM eval (varindices m)
                   midsv <- index mv mids
                   nidsv <- index nv nids
                   nv' <- updateIndex nv nids midsv
                   mv' <- updateIndex mv mids nidsv
                   setStore (Map.insert (varbase n) nv' (Map.insert (varbase m) mv' sym))
                   sym' <- getStore
                   nextStmt

-- Local
interpretComStmt (Local p t ns e) = do
  sym <- getStore
  v' <- eval e
  let v = if v' == ComValue Empty then type2symbol t
                                  else v'
  vns <- Map.fromList . zip ns <$> mapM (evalVar . flip (Var p) []) ns
  let (vnse,vnsf) = Map.partition (== ComValue Empty) vns
  Monad.unless (null vnsf) $ do
    let (n,vf) = head (Map.toList vnsf)
    Except.throwError . ComInterpreterError $ NonEmpty (Var p n []) vf
  setStore $ Map.unionWith (\_ v->v) sym (Map.map (const v) vnse)
  nextStmt


--      where registerVar n = do
--              case Map.lookup n sym of
--                Just (ComValue Empty) -> setStore (Map.insert n v sym)
--                Just vf -> Except.throwError . ComInterpreterError $ NonEmpty (Var p n []) vf
--                Nothing -> Except.throwError . ComInterpreterError $ UnknownVariable (Var p n [])
--    
-- Delocal
interpretComStmt (Delocal p t ns e) = do
  sym <- getStore
  v'' <- eval e
  let v = if v'' == ComValue Empty then type2symbol t
                                   else v''
  vns <- Map.fromList . zip ns <$> mapM (evalVar . flip (Var p) []) ns
  let (vnst,vnsf) = Map.partition (==v) vns
  Monad.unless (null vnsf) $ do
    let (n,vf) = head (Map.toList vnsf)
    Except.throwError . ComInterpreterError $ DelocalUnexpectedValue n v vf
  setStore $ Map.unionWith (\_ v->v) sym (Map.map (const $ ComValue Empty) vnst)
  nextStmt

-- Update
interpretComStmt (Update p n op e) = do -- TODO: not working correctly
  selfReferencing n e
  vno  <- evalVar n { varindices = [] }
  vn   <- evalVar n
  ve   <- eval e
  vn'  <- reduceFracVal <$> evalBinOp op vn ve True
  nids <- mapM eval (varindices n)
  vno' <- updateIndex vno nids vn'
  getStore >>= setStore . Map.insert (varbase n) vno'
  nextStmt

-- Push
interpretComStmt (Push p n m) = evalVar m >>= \case
  ComValue (ListVal vm) -> do
    nids <- mapM eval (varindices n)
    mids <- mapM eval (varindices m)
    vno  <- evalVar n { varindices = [] }
    vmo  <- evalVar m { varindices = [] }
    (vno',v) <- removeIndex vno nids >>= \case
            (ComValue Empty,v) -> (,v) <$> emptyValue n
            vnv -> return vnv
    vmo' <- updateIndex vmo mids (ComValue . ListVal $ v:vm)
    getStore >>= setStore . Map.insert (varbase n) vno' . Map.insert (varbase m) vmo'
    sym <- getStore
    nextStmt
  vm -> Except.throwError . ComInterpreterError $ NonListValue m vm

-- Pop
interpretComStmt (Pop p n m) = evalVar m >>= \case
  ComValue (ListVal (v:vs)) -> do
    evn <- emptyValue n
    evalVar n >>= \case
      vn | vn == evn -> do
        nids <- mapM eval (varindices n)
        mids <- mapM eval (varindices m)
        vno <- evalVar $ n { varindices = [] }
        vmo <- evalVar $ m { varindices = [] }
        vno' <- updateIndex vno nids v
        vmo' <- updateIndex vmo mids (ComValue (ListVal vs))
        getStore >>= setStore . Map.insert (varbase n) vno' . Map.insert (varbase m) vmo'
        nextStmt
      vn -> Except.throwError . ComInterpreterError $ NonEmpty n vn
  ComValue (ListVal []) -> Except.throwError . ComInterpreterError $ EmptyListValue m
  vf -> Except.throwError . ComInterpreterError $ NonListValue m vf 

-- Enqueue
interpretComStmt (Enqueue p n q) = do
  vno <- evalVar n { varindices = [] }
  vqo <- evalVar q { varindices = [] }
  nids <- mapM eval (varindices n)
  (vno',v) <- removeIndex vno nids >>= \case
          (ComValue Empty,v) -> (,v) <$> emptyValue n
          vnv -> return vnv
  evalVar q >>= \case
    ComValue (QueueVal vq) -> do
      qids <- mapM eval (varindices q)
      vqo' <- updateIndex vqo qids (ComValue . QueueVal $ Queue.enqueue v vq)
      getStore >>= setStore . Map.insert (varbase n) vno' . Map.insert (varbase q) vqo'
      nextStmt
    vf -> Except.throwError . ComInterpreterError $ NonQueueValue q vf 

-- Dequeue
interpretComStmt (Dequeue p n q) = evalVar q >>= \case
  ComValue (QueueVal vq)
    | null vq -> Except.throwError . ComInterpreterError $ EmptyQueueValue q
    | otherwise -> do
      evn <- emptyValue n
      evalVar n >>= \case
        vn | vn /= evn -> Except.throwError . ComInterpreterError $ NonEmpty n vn
           | otherwise -> do
             nids <- mapM eval (varindices n)
             qids <- mapM eval (varindices q)
             let (Just v,vq') = Queue.dequeue vq
             vqo  <- evalVar q { varindices = [] }
             vno  <- evalVar n { varindices = [] }
             vno' <- updateIndex vno nids v
             vqo' <- updateIndex vqo qids (ComValue . QueueVal $ vq')
             getStore >>= setStore . Map.insert (varbase n) vno' . Map.insert (varbase q) vqo'
             nextStmt
  vf -> Except.throwError . ComInterpreterError $ NonQueueValue q vf 

-- Undequeue
interpretComStmt (Undequeue p n q) = do
  vno <- evalVar n { varindices = [] }
  vqo <- evalVar q { varindices = [] }
  nids <- mapM eval (varindices n)
  (vno',v) <- removeIndex vno nids >>= \case
          (ComValue Empty,v) -> (,v) <$> emptyValue n
          vnv -> return vnv
  evalVar q >>= \case
    ComValue (QueueVal vq) -> do
      qids <- mapM eval (varindices q)
      vqo' <- updateIndex vqo qids (ComValue . QueueVal $ Queue.undequeue v vq)
      getStore >>= setStore . Map.insert (varbase n) vno' . Map.insert (varbase q) vqo'
      nextStmt
    vf -> Except.throwError . ComInterpreterError $ NonQueueValue q vf 

-- Unenqueue
interpretComStmt (Unenqueue p n q) = evalVar q >>= \case
  ComValue (QueueVal vq)
    | null vq -> Except.throwError . ComInterpreterError $ EmptyQueueValue q
    | otherwise -> do
      evn <- emptyValue n
      evalVar n >>= \case
        vn | vn /= evn -> Except.throwError . ComInterpreterError $ NonEmpty n vn
           | otherwise -> do
             nids <- mapM eval (varindices n)
             qids <- mapM eval (varindices q)
             let (Just v,vq') = Queue.unenqueue vq
             vqo  <- evalVar q { varindices = [] }
             vno  <- evalVar n { varindices = [] }
             vno' <- updateIndex vno nids v
             vqo' <- updateIndex vqo qids (ComValue . QueueVal $ vq')
             getStore >>= setStore . Map.insert (varbase n) vno' . Map.insert (varbase q) vqo'
             nextStmt
  vf -> Except.throwError . ComInterpreterError $ NonQueueValue q vf 

-- If
interpretComStmt (If p e1 s1 s2 e2 _) = do
  v1 <- eval e1
  let (ss,b) = if value2bool v1 then (s1,True) else (s2,False)
  removeStmt >> insertStmt (ComStmt $ If p e1 s1 s2 e2 b)
  addLayer (Zipper.begin ss)

-- Loop
interpretComStmt s@(Loop p e1 s1 s2 e2 _) = eval e1 >>= \case
  v1 | not (value2bool v1) -> Except.throwError . ComInterpreterError $ AssertionFailed e1 True False
     | otherwise -> do
       removeStmt >> insertStmt (ComStmt $ Loop p e1 s1 s2 e2 True)
       addLayer (Zipper.begin s1)

-- Call
interpretComStmt (Call p f as) = do
  multiReferencingArgs p as
  psym <- getProcedureTable
  caller <- getEnv
  fn <- evalVar f >>= \case
    ComValue (RefFuncVal _ f') -> return $ Map.lookup f' psym
    _ -> return Nothing
  case fn of
    Just callee@(ComProcedure _) -> do
      -- Compute bindings
      let calleeBindings = Map.fromList $ zip (map fst $ params callee) (Maybe.mapMaybe (`Map.lookup` store caller) as)
      let calleeSym = Map.unionWith (\_ v->v) (store callee) calleeBindings
      let callerBindings = Map.fromList $ zip as (map (const $ ComValue Empty) as)
      let callerSym = Map.unionWith (\_ v->v) (store caller) callerBindings
      -- Update caller and add to stack
      setStore callerSym
      caller' <- getEnv
      getStack >>= setStack . (caller':)
      -- Update callee
      setEnv callee
      setStore calleeSym
      nextStmt
    _ -> Except.throwError . ComInterpreterError $ UnknownVariable f

interpretComStmt (Uncall p f as) = do
  multiReferencingArgs p as
  psym <- getProcedureTable
  caller <- getEnv
  fn <- evalVar f >>= \case
    ComValue (RefFuncVal _ f') -> return $ Map.lookup f' psym
    _ -> return Nothing
  case fn of
    Just callee@(ComProcedure _) -> do
      -- Compute bindings
      let calleeBindings = Map.fromList $ zip (map fst $ params callee) (Maybe.mapMaybe (`Map.lookup` store caller) as)
      let calleeSym = Map.unionWith (\_ v->v) (store callee) calleeBindings
      let callerBindings = Map.fromList $ zip as (map (const $ ComValue Empty) as)
      let callerSym = Map.unionWith (\_ v->v) (store caller) callerBindings
      -- Update caller and add to stack
      setStore callerSym
      caller' <- getEnv
      getStack >>= setStack . (caller':)
      -- Update callee
      setEnv $ inverseCallee callee
      setStore calleeSym
      nextStmt
    _ -> Except.throwError . ComInterpreterError $ UnknownVariable f
  where inverseCallee (ComProcedure f) = Inverter.inverseProcedure $ ComProcedure $ f { funcbody = map Zipper.end (funcbody f) }

interpretComStmt FunctionReturn = getStack >>= \case
  [] -> Except.throwError . ComInterpreterError $ EmptyStack
  caller:stack -> do
    -- Update stack
    setStack stack
    -- Check if at Call or Uncall in caller
    callee <- getEnv
    setEnv caller
    (f,as) <- getStmt >>= \case
      Just (ComStmt (Call _ f as))   -> return (f,as)
      Just (ComStmt (Uncall _ f as)) -> return (f,as)
      Just s -> Except.throwError . ComInterpreterError $ NonLayeredStmt s
      Nothing -> getEnv >>= Except.throwError . ComInterpreterError . NoMoreStatements . name
    -- Check if empty local state
    let calleeBindings = Map.fromList $ zip (map fst (params callee)) (Maybe.mapMaybe ((`Map.lookup` store callee) . fst) (params callee))
    let nonempty = Map.filter (ComValue Empty /=) $ Map.difference (store callee) calleeBindings
    Monad.unless (null nonempty) (Except.throwError . ComInterpreterError $ NonEmptyLocalStore (varbase f) nonempty)
    -- Compute bindings
    let callerBindings = Map.fromList $ zip as (Maybe.mapMaybe ((`Map.lookup` store callee) . fst) (params callee))
    let callerSym = Map.unionWith (\_ v->v) (store caller) callerBindings
    setStore callerSym
    nextStmt

interpretComStmt _ = nextStmt

-- Return from processed layer
returnLayer :: (VariantInterpreter v) => InterpreterState v ()
returnLayer = do
  (layer,ls) <- getCurrentLayers
  p <- State.get
  setCurrentLayers ls
  case ls of
    [] -> Except.throwError . ComInterpreterError $ NoReturnLayer
    retlayer:ls' -> do
      case Zipper.cursor retlayer of
        Just (ComStmt s) -> returnComLayer s layer
        Just (VarStmt s) -> returnVarLayer s layer
        Nothing -> getEnv >>= Except.throwError . ComInterpreterError . NoMoreStatements . name

returnComLayer :: (VariantInterpreter v) => ComStmt v -> Stmts v -> InterpreterState v ()
-- If
returnComLayer (If p e1 _ s2 e2 True)  s1 = eval e2 >>= \case
  v2 | value2bool v2 -> removeStmt >> insertStmt (ComStmt $ If p e1 s1 s2 e2 True) >> nextStmt
     | otherwise     -> Except.throwError . ComInterpreterError $ AssertionFailed e1 True False
returnComLayer (If p e1 s1 _ e2 False) s2 = eval e2 >>= \case
  v2 | not (value2bool v2) -> removeStmt >> insertStmt (ComStmt $ If p e1 s1 s2 e2 False) >> nextStmt
     | otherwise           -> Except.throwError . ComInterpreterError $ AssertionFailed e1 False True
-- Loop
returnComLayer (Loop p e1 _ s2 e2 True)  s1 = eval e2 >>= \case
  v2 | value2bool v2 -> do
      removeStmt >> insertStmt (ComStmt $ Loop p e1 s1 s2 e2 True) >> nextStmt
     | otherwise     -> do
      removeStmt >> insertStmt (ComStmt $ Loop p e1 s1 s2 e2 False)
      addLayer (Zipper.begin s2)
returnComLayer (Loop p e1 s1 _ e2 False)  s2 = eval e1 >>= \case
  v1 | value2bool v1 -> Except.throwError . ComInterpreterError $ AssertionFailed e1 False True
     | otherwise     -> do
      removeStmt >> insertStmt (ComStmt $ Loop p e1 s1 s2 e2 True)
      addLayer (Zipper.begin s1)


-- Expressions
eval :: (VariantInterpreter v) => Expr v -> InterpreterState v (Value v)
eval (BinOp op e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  evalBinOp op v1 v2 False
eval (UnOp op e) = eval e >>= evalUnOp op
eval (Value v) = reduceFracVal <$> evalValue v

evalBinOp :: (VariantInterpreter v) => BinOp v -> Value v -> Value v -> Bool -> InterpreterState v (Value v)
evalBinOp (ComBinOp op) v1 v2 b = evalComBinOp op v1 v2 b
evalBinOp (VarBinOp op) v1 v2 b = evalVarBinOp op v1 v2 b

evalComBinOp :: (VariantInterpreter v) => ComBinOp v -> Value v -> Value v -> Bool -> InterpreterState v (Value v)
-- Plus
evalComBinOp Plus (ComValue (IntVal v1)) (ComValue (IntVal v2)) _ = return . ComValue . IntVal $ v1 + v2
evalComBinOp Plus (ComValue (FracVal v11 v12)) (ComValue (IntVal v2)) _ = return . ComValue $ FracVal (v11 + v2 * v12) v12
evalComBinOp Plus (ComValue (IntVal v1)) (ComValue (FracVal v21 v22)) _ = return . ComValue $ FracVal (v21 + v1 * v22) v22
evalComBinOp Plus (ComValue (FracVal v11 v12)) (ComValue (FracVal v21 v22)) _ = return . ComValue $ FracVal (v11 * v22 + v21 * v12) (v12 * v22)

-- Minus
evalComBinOp Minus (ComValue (IntVal v1)) (ComValue (IntVal v2)) _ = return . ComValue . IntVal $ v1 - v2
evalComBinOp Minus (ComValue (FracVal v11 v12)) (ComValue (IntVal v2)) _ = return . ComValue $ FracVal (v11 - v2 * v12) v12
evalComBinOp Minus (ComValue (IntVal v1)) (ComValue (FracVal v21 v22)) _ = return . ComValue $ FracVal (v21 - v1 * v22) v22
evalComBinOp Minus (ComValue (FracVal v11 v12)) (ComValue (FracVal v21 v22)) _ = return . ComValue $ FracVal (v11 * v22 - v21 * v12) (v12 * v22)

-- Times
evalComBinOp Times _ v2 True | not (value2bool v2) = getStmt >>= Except.throwError . ComInterpreterError . ZeroMultiplication . Maybe.fromJust
evalComBinOp Times (ComValue (IntVal v1))       (ComValue (IntVal v2)) _ = return . ComValue . IntVal $ v1 * v2
evalComBinOp Times (ComValue (FracVal v11 v12)) (ComValue (IntVal v2)) _ = return . ComValue $ FracVal (v11 * v2) v12
evalComBinOp Times (ComValue (IntVal v1))       (ComValue (FracVal v21 v22)) _ = return . ComValue $ FracVal (v21 * v1) v22
evalComBinOp Times (ComValue (FracVal v11 v12)) (ComValue (FracVal v21 v22)) _ = return . ComValue $ FracVal (v11 * v21) (v12 * v22)

-- Divide
evalComBinOp Divide _ v2 _ | not (value2bool v2) = getStmt >>= Except.throwError . ComInterpreterError . ZeroDivision . Maybe.fromJust
evalComBinOp Divide (ComValue (IntVal v1))       (ComValue (IntVal v2)) _ = return . ComValue $ FracVal v1 v2
evalComBinOp Divide (ComValue (FracVal v11 v12)) (ComValue (IntVal v2)) _ = return . ComValue $ FracVal v11 (v12 * v2)
evalComBinOp Divide (ComValue (IntVal v1))       (ComValue (FracVal v21 v22)) _ = return . ComValue $ FracVal (v1 * v22) v21
evalComBinOp Divide (ComValue (FracVal v11 v12)) (ComValue (FracVal v21 v22)) _ = return . ComValue $ FracVal (v11 * v22) (v12 * v21)

-- XOR
evalComBinOp Xor (ComValue (IntVal v1)) (ComValue (IntVal v2)) _ = return . ComValue . IntVal $ Bits.xor v1 v2
-- TODO: implement for fracs

-- Equal
evalComBinOp Equal v1 v2 _ = return . ComValue $ if v1 == v2 then IntVal 1 else IntVal 0

-- Not equal
evalComBinOp NotEqual v1 v2 _ = return . ComValue $ if v1 /= v2 then IntVal 1 else IntVal 0

-- Greater
evalComBinOp Greater (ComValue (IntVal v1)) (ComValue (IntVal v2)) _ = return . ComValue $ if v1 > v2 then IntVal 1 else IntVal 0
evalComBinOp Greater (ComValue (FracVal v11 v12)) (ComValue (IntVal v2)) _
  | v12 < 0   = return . ComValue $ if v11 < v2 * v12 then IntVal 1 else IntVal 0
  | otherwise = return . ComValue $ if v11 > v2 * v12 then IntVal 1 else IntVal 0
evalComBinOp Greater (ComValue (IntVal v1)) (ComValue (FracVal v21 v22)) _
  | v22 < 0   = return . ComValue $ if v1 * v22 < v21 then IntVal 1 else IntVal 0
  | otherwise = return . ComValue $ if v1 * v22 > v21 then IntVal 1 else IntVal 0
evalComBinOp Greater (ComValue (FracVal v11 v12)) (ComValue (FracVal v21 v22)) _
  | v22 < 0 && v12 > 0 = return . ComValue $ if v11 * v22 < v21 * v12 then IntVal 1 else IntVal 0
  | v12 < 0 && v22 > 0 = return . ComValue $ if v11 * v22 < v21 * v12 then IntVal 1 else IntVal 0
  | otherwise = return . ComValue $ if v11 * v22 > v21 * v12 then IntVal 1 else IntVal 0

-- Lesser
evalComBinOp Lesser (ComValue (IntVal v1)) (ComValue (IntVal v2)) _ = return . ComValue $ if v1 < v2 then IntVal 1 else IntVal 0
evalComBinOp Lesser (ComValue (FracVal v11 v12)) (ComValue (IntVal v2)) _
  | v12 < 0   = return . ComValue $ if v11 > v2 * v12 then IntVal 1 else IntVal 0
  | otherwise = return . ComValue $ if v11 < v2 * v12 then IntVal 1 else IntVal 0
evalComBinOp Lesser (ComValue (IntVal v1)) (ComValue (FracVal v21 v22)) _
  | v22 < 0   = return . ComValue $ if v1 * v22 > v21 then IntVal 1 else IntVal 0
  | otherwise = return . ComValue $ if v1 * v22 < v21 then IntVal 1 else IntVal 0
evalComBinOp Lesser (ComValue (FracVal v11 v12)) (ComValue (FracVal v21 v22)) _
  | v22 < 0 && v12 > 0 = return . ComValue $ if v11 * v22 > v21 * v12 then IntVal 1 else IntVal 0
  | v12 < 0 && v22 > 0 = return . ComValue $ if v11 * v22 > v21 * v12 then IntVal 1 else IntVal 0
  | otherwise = return . ComValue $ if v11 * v22 < v21 * v12 then IntVal 1 else IntVal 0

-- And
evalComBinOp And v1 v2 _ = return . ComValue $ if value2bool v1 && value2bool v2 then IntVal 1 else IntVal 0

-- Or
evalComBinOp Or v1 v2 _ = return . ComValue $ if value2bool v1 || value2bool v2 then IntVal 1 else IntVal 0

evalComBinOp op v1 v2 _ = undefined `debug` (show v1 ++ " " ++ show op ++ " " ++ show v2)

evalUnOp :: (VariantInterpreter v) => UnOp v -> Value v -> InterpreterState v (Value v)
evalUnOp (ComUnOp op) v = evalComUnOp op v
evalUnOp (VarUnOp op) v = evalVarUnOp op v

evalComUnOp :: (VariantInterpreter v) => ComUnOp v -> Value v -> InterpreterState v (Value v)
-- Not
evalComUnOp Not (ComValue (IntVal 0)) = return . ComValue $ IntVal 0
evalComUnOp Not (ComValue (IntVal _)) = return . ComValue $ IntVal 1

-- Size
evalComUnOp Size (ComValue (ListVal v)) = return . ComValue . IntVal $ length v
evalComUnOp Size (ComValue (ListLit v)) = return . ComValue . IntVal $ length v
evalComUnOp Size (ComValue (QueueVal v)) = return . ComValue . IntVal . length $ Queue.toList v

-- Hea
evalComUnOp Head (ComValue (ListVal [])) = Except.throwError . ComInterpreterError $ EmptyListOp (ComUnOp Head)
evalComUnOp Head (ComValue (ListLit [])) = Except.throwError . ComInterpreterError $ EmptyListOp (ComUnOp Head)
evalComUnOp Head (ComValue (ListVal v)) = return $ head v
evalComUnOp Head (ComValue (ListLit v)) = eval $ head v

-- Head
evalComUnOp Last (ComValue (ListVal [])) = Except.throwError . ComInterpreterError $ EmptyListOp (ComUnOp Last)
evalComUnOp Last (ComValue (ListLit [])) = Except.throwError . ComInterpreterError $ EmptyListOp (ComUnOp Last)
evalComUnOp Last (ComValue (ListVal v)) = return $ last v
evalComUnOp Last (ComValue (ListLit v)) = eval $ last v

evalComUnOp UnOpEmpty (ComValue (ListVal [])) = return . ComValue $ IntVal 1
evalComUnOp UnOpEmpty (ComValue (ListVal _))  = return . ComValue $ IntVal 0
evalComUnOp UnOpEmpty (ComValue (ListLit [])) = return . ComValue $ IntVal 1
evalComUnOp UnOpEmpty (ComValue (ListLit _))  = return . ComValue $ IntVal 0
evalComUnOp UnOpEmpty (ComValue (QueueVal q))
  | null q    = return . ComValue $ IntVal 1
  | otherwise = return . ComValue $ IntVal 0

evalValue :: (VariantInterpreter v) => Value v -> InterpreterState v (Value v)
evalValue (ComValue (RefVal n)) = evalVar n
evalValue (ComValue (ListLit vs)) = ComValue . ListVal <$> mapM eval vs
evalValue v = return v

evalVar :: (VariantInterpreter v) => Var v -> InterpreterState v (Value v)
evalVar (Var p ('@':n) []) = return . ComValue $ RefFuncVal p n
evalVar no@(Var p n ids) = do
  sym <- getStore
  ls <- mapM eval ids
  case Map.lookup n sym of
    Just v -> index v ls
    Nothing -> Except.throwError . ComInterpreterError $ UnknownVariable no

-- Frac computation
reduceFracVal :: (VariantInterpreter v) => Value v -> Value v
reduceFracVal (ComValue (FracVal v1 v2))
  | v1 `mod` v2 == 0 = ComValue . IntVal $ v1 `div` v2
  | otherwise = let d = gcd v1 v2 in ComValue $ FracVal (v1 `div` d) (v2 `div` d)
reduceFracVal v = v

-- Var indexing
index :: (VariantInterpreter v) => Value v -> [Value v] -> InterpreterState v (Value v)
index vo@(ComValue (ListVal v)) (ComValue (IntVal id):ids)
  | id < length v = index (v !! id) ids
  | otherwise     = Except.throwError . ComInterpreterError $ ListOutOfBounds vo id
index vo@(ComValue (ListVal v)) (id@(ComValue FracVal{}):ids) =
  case reduceFracVal id of
    ComValue (IntVal id') | id' < length v -> index (v !! id') ids
                          | otherwise -> Except.throwError . ComInterpreterError $ ListOutOfBounds vo id'
    i' -> Except.throwError . ComInterpreterError $ NonIntegerIndex vo i'
index v [] = return v
index v l  = Except.throwError . ComInterpreterError $ NonListIndex v l

-- Var index update
updateIndex :: (VariantInterpreter v) => Value v -> [Value v] -> Value v -> InterpreterState v (Value v)
updateIndex v [] w = return w
updateIndex vo@(ComValue (ListVal v)) ((ComValue (IntVal id)):ids) w
  | id < length v = do
    vid' <- updateIndex (v !! id) ids w
    return . ComValue . ListVal $ take id v ++ vid':drop (id+1) v
  | otherwise    = Except.throwError . ComInterpreterError $ ListOutOfBounds vo id
updateIndex vo@(ComValue (ListVal v)) (id@(ComValue FracVal{}):ids) w =
  case reduceFracVal id of
    ComValue (IntVal id') | id' < length v -> do
                            vid' <- updateIndex (v !! id') ids w
                            return . ComValue . ListVal $ take id' v ++ vid':drop (id'+1) v
                          | otherwise -> Except.throwError . ComInterpreterError $ ListOutOfBounds vo id'
    i' -> Except.throwError . ComInterpreterError $ NonIntegerIndex vo i'
updateIndex v l _ = Except.throwError . ComInterpreterError $ NonListIndex v l

-- Var index remove
removeIndex :: (VariantInterpreter v) => Value v -> [Value v] -> InterpreterState v (Value v,Value v)
removeIndex v [] = return (ComValue Empty,v)
removeIndex vo@(ComValue (ListVal v)) [ComValue (IntVal id)]
  | id < length v = return (ComValue . ListVal $ take id v ++ drop (id+1) v, v !! id)
  | otherwise    = Except.throwError . ComInterpreterError $ ListOutOfBounds vo id
removeIndex vo@(ComValue (ListVal v)) [id@(ComValue FracVal{})] =
  case reduceFracVal id of
    ComValue (IntVal id') | id' < length v -> return (ComValue . ListVal $ take id' v ++ drop (id'+1) v, v !! id')
                          | otherwise -> Except.throwError . ComInterpreterError $ ListOutOfBounds vo id'
    id' -> Except.throwError . ComInterpreterError $ NonIntegerIndex vo id'
removeIndex vo@(ComValue (ListVal v)) ((ComValue (IntVal id)):ids)
  | id < length v = do
    (vid',x) <- removeIndex (v !! id) ids
    return (ComValue . ListVal $ take id v ++ vid':drop (id+1) v,x)
  | otherwise    = Except.throwError . ComInterpreterError $ ListOutOfBounds vo id
removeIndex vo@(ComValue (ListVal v)) (id@(ComValue FracVal{}):ids) =
  case reduceFracVal id of
    ComValue (IntVal id') | id' < length v -> do
                            (vid',x) <- removeIndex (v !! id') ids
                            return (ComValue . ListVal $ take id' v ++ vid':drop (id'+1) v,x)
                          | otherwise -> Except.throwError . ComInterpreterError $ ListOutOfBounds vo id'
    id' -> Except.throwError . ComInterpreterError $ NonIntegerIndex vo id'
removeIndex v l = Except.throwError . ComInterpreterError $ NonListIndex v l

-- Num Instances
value2bool :: Value v -> Bool
value2bool (ComValue (IntVal 0))    = False
value2bool (ComValue (FracVal 0 _)) = False
value2bool _                        = True

-- Self referencing
selfReferencing :: (VariantInterpreter v) => Var v -> Expr v -> InterpreterState v ()
selfReferencing n (BinOp _ e1 e2) = do
  selfReferencing n e1
  selfReferencing n e2
selfReferencing n (UnOp op e) = selfReferencing n e
selfReferencing n (Value v) = selfReferencingValue n v

selfReferencingValue :: (VariantInterpreter v) => Var v -> Value v -> InterpreterState v ()
selfReferencingValue n (ComValue v) = selfReferencingComValue n v
selfReferencingValue n (VarValue v) = selfReferencingVarValue n v

selfReferencingComValue :: (VariantInterpreter v) => Var v -> ComValue v -> InterpreterState v ()
selfReferencingComValue n (RefVal m)
  | varbase n == varbase m = do
   nids <- mapM eval (varindices n)
   mids <- mapM eval (varindices m)
   if mids == nids then (getStmt >>= Except.throwError . ComInterpreterError . SelfReferencing n . Maybe.fromJust)
                   else return ()
 -- | n == m = getStmt >>= Except.throwError . ComInterpreterError . SelfReferencing n . Maybe.fromJust -- TODO: Check for list indexing when implemented updating on indeices
selfReferencingComValue n _ = return ()

-- self-frequency
multiReferencingArgs :: (VariantInterpreter v) => Pos -> Args -> InterpreterState v ()
multiReferencingArgs p as = case filter ((>1) . snd) (frequency as) of
                           [] -> return ()
                           as' -> Except.throwError . ComInterpreterError $ MultiReferencingArgs p (map fst as')
  where frequency :: (Ord a) => [a] -> [(a, Int)]
        frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])
