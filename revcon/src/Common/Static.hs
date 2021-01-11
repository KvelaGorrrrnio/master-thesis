{-# LANGUAGE AllowAmbiguousTypes, PolyKinds, FlexibleContexts, UndecidableInstances, LambdaCase #-}
module Common.Static where

import Common.Classes
import Common.Types
import Common.Error

import qualified Common.Zipper as Zipper
import qualified Common.Queue as Queue
import qualified Data.Map as Map
import qualified Control.Monad.State as State (execState)

import Control.Monad.State
import Data.Maybe (fromMaybe)

-- Set types
settypesProcedure :: (VariantTypeChecker v) => Procedure v -> ProcedureTable v -> Procedure v
settypesProcedure (ComProcedure p) pt = ComProcedure (settypesComProcedure p pt)
settypesProcedure (VarProcedure p) pt = VarProcedure (settypesVarProcedure p pt)

settypesLayers :: (VariantTypeChecker v) => [Stmts v] -> State (TypeTable v, ProcedureTable v) ()
settypesLayers = foldr ((>>) . settypesStmts) (return ())


settypesComProcedure :: (VariantTypeChecker v) => ComProcedure v -> ProcedureTable v -> ComProcedure v
settypesComProcedure f pt = f { functypes = fst $ State.execState (settypesLayers (funcbody f)) (Map.fromList (funcparams f), pt) }

settypesStmts :: (VariantTypeChecker v) => Stmts v -> State (TypeTable v, ProcedureTable v) ()
settypesStmts = foldr ((>>) . settypesStmt) (return ())

settypesStmt :: (VariantTypeChecker v) => Stmt v -> State (TypeTable v, ProcedureTable v) ()
settypesStmt (ComStmt s) = settypesComStmt s
settypesStmt (VarStmt s) = settypesVarStmt s

settypesComStmt :: (TypedVariant v,VariantTypeChecker v) => ComStmt v -> State (TypeTable v, ProcedureTable v) ()
settypesComStmt (Local _ t ns _) = foldr ((>>) . registerType t) (return ()) ns
    
settypesComStmt (If _ _ s1 s2 _ _) = do
  settypesStmts s1
  settypesStmts s2

settypesComStmt (Loop _ _ s1 s2 _ _) = do
  settypesStmts s1
  settypesStmts s2

settypesComStmt _ = return ()

-- Type to Symbol
type2symbol :: (VariantTypeChecker v) => VarType v -> Value v
type2symbol (ComVarType t) = comVarType2symbol t
type2symbol (VarVarType t) = varVarType2symbol t

comVarType2symbol :: (VariantTypeChecker v) => ComVarType v -> Value v
comVarType2symbol (ListT t)  = ComValue . ListVal $ []
comVarType2symbol (QueueT t)  = ComValue . QueueVal $ Queue.empty
comVarType2symbol NumT       = ComValue . IntVal $ 0
comVarType2symbol (FunT _)   = ComValue Empty
comVarType2symbol Unknown    = ComValue Empty


-- Type check
typecheckProcedure :: (VariantTypeChecker v) => Procedure v -> TypeCheckState v ()
typecheckProcedure (ComProcedure p) = typecheckComProcedure p
typecheckProcedure (VarProcedure p) = typecheckVarProcedure p

typecheckComProcedure :: (VariantTypeChecker v) => ComProcedure v -> TypeCheckState v ()
typecheckComProcedure c = withTypeTable (functypes c) >> typecheckLayers (funcbody c) >> withTypeTable Map.empty
    where withTypeTable t = get >>= \(s,_) -> put (s,t)
          typecheckLayers ls = foldr ((>>) . typecheckStmts) (return ()) ls

typecheckStmts :: (VariantTypeChecker v) => Stmts v -> TypeCheckState v ()
typecheckStmts = foldr ((>>) . typecheckStmt) (return ())

typecheckStmt :: (VariantTypeChecker v) => Stmt v -> TypeCheckState v ()
typecheckStmt (ComStmt s) = typecheckComStmt s
typecheckStmt (VarStmt s) = typecheckVarStmt s

typecheckComStmt :: (VariantTypeChecker v) => ComStmt v -> TypeCheckState v ()
-- Swap
typecheckComStmt (Swap _ n m) = do
  tn <- typeof n
  tm <- typeof m
  case compareTypes tn tm of
    Just _  -> return ()
    Nothing -> addError $ ComTypeError $ UnexpectedType n tm tn

-- Update
typecheckComStmt (Update pos n op e) = do
  tn <- typeof n
  te <- typecheckExpr e
  case compareTypes tn te of
    Just t | tn == t   -> return ()
           | otherwise -> return ()
    Nothing -> addError $ ComTypeError $ UnexpectedExprType pos tn te

-- If
typecheckComStmt (If pos e1 s1 s2 e2 _) = do
  t1 <- typecheckExpr e1
  t2 <- typecheckExpr e2
  case (t1,t2) of
    (ComVarType NumT, ComVarType NumT) -> return ()
    (ComVarType NumT,t)               -> addError $ ComTypeError $ UnexpectedExprType pos (ComVarType NumT) t
    (t,ComVarType NumT  )             -> addError $ ComTypeError $ UnexpectedExprType pos (ComVarType NumT) t
    (t1,t2) -> do
      addError $ ComTypeError $ UnexpectedExprType pos (ComVarType NumT) t1
      addError $ ComTypeError $ UnexpectedExprType pos (ComVarType NumT) t2
  typecheckStmts s1
  typecheckStmts s2

-- Loop
typecheckComStmt (Loop pos e1 s1 s2 e2 _) = do
  t1 <- typecheckExpr e1
  t2 <- typecheckExpr e2
  case (t1,t2) of
    (ComVarType NumT, ComVarType NumT) -> return ()
    (ComVarType NumT,t)               -> addError $ ComTypeError $ UnexpectedExprType pos (ComVarType NumT) t
    (t,ComVarType NumT  )             -> addError $ ComTypeError $ UnexpectedExprType pos (ComVarType NumT) t
    (t1,t2)     -> do
      addError $ ComTypeError $ UnexpectedExprType pos (ComVarType NumT) t1
      addError $ ComTypeError $ UnexpectedExprType pos (ComVarType NumT) t2
  typecheckStmts s1
  typecheckStmts s2

-- Call
typecheckComStmt (Call _ f as) = do
  t <- case varbase f of
         '@':f' -> typeofProc f'
         _      -> typeof f
  case t of
    (ComVarType (FunT ts)) ->
      if length ts == length as
         then typecheckArgs ts as
         else addError . ComTypeError $ IncorrectArgNumber f (length ts) (length as)
    _ -> do
      ts <- mapM (typeof . flip (Var (-1,-1)) []) as
      addError . ComTypeError $ UnexpectedType f (ComVarType (FunT ts)) t

-- Uncall
typecheckComStmt (Uncall _ f as) = do
  t <- case varbase f of
         '@':f' -> typeofProc f'
         _      -> typeof f
  case t of
    (ComVarType (FunT ts)) ->
      if length ts == length as
         then typecheckArgs ts as
         else addError . ComTypeError $ IncorrectArgNumber f (length ts) (length as)
    _ -> do
      ts <- mapM (typeof . flip (Var (-1,-1)) []) as
      addError . ComTypeError $ UnexpectedType f (ComVarType (FunT ts)) t


-- Pop
typecheckComStmt (Pop p n m) = typecheckComStmt (Push p n m)
-- Push
typecheckComStmt (Push _ n m) = do
  tn <- typeof n
  tm <- typeof m
  case compareTypes (ComVarType $ ListT tn) tm of
    Just _ -> return ()
    Nothing -> addError $ ComTypeError $ UnexpectedType m (ComVarType $ ListT tn) tm

-- Enqueue
typecheckComStmt (Enqueue _ n m) = do
  tn <- typeof n
  tm <- typeof m
  case compareTypes (ComVarType $ QueueT tn) tm of
    Just _ -> return ()
    Nothing -> addError $ ComTypeError $ UnexpectedType m (ComVarType $ QueueT tn) tm
-- Unenqueue
typecheckComStmt (Unenqueue p n m) = typecheckComStmt (Enqueue p n m)
-- Dequeue
typecheckComStmt (Dequeue p n m) = typecheckComStmt (Enqueue p n m)
-- Undequeue
typecheckComStmt (Undequeue p n m) = typecheckComStmt (Enqueue p n m)

-- Local
typecheckComStmt (Delocal pos t n e) = typecheckComStmt (Local pos t n e)
typecheckComStmt (Local pos t _ e) = do
  te <- typecheckExpr e
  case compareTypes t te of
    Just t' -> return ()
    Nothing -> 
      case e of
        Value (ComValue Empty) -> return ()
        _ -> addError $ ComTypeError $ UnexpectedExprType pos t te

typecheckComStmt _ = return ()

typecheckExpr :: (VariantTypeChecker v) => Expr v -> TypeCheckState v (VarType v)
typecheckExpr (BinOp op' v1 v2) = do
  t1 <- typecheckExpr v1
  t2 <- typecheckExpr v2
  case op' of
    (ComBinOp op) -> typecheckComBinOp op t1 t2
    (VarBinOp op) -> typecheckVarBinOp op t1 t2
typecheckExpr (UnOp op' v) = do
  t <- typecheckExpr v
  case op' of
    (ComUnOp op) -> typecheckComUnOp op t
    (VarUnOp op) -> typecheckVarUnOp op t
typecheckExpr (Value (ComValue v)) = typecheckComValue v

typecheckComBinOp :: (VariantTypeChecker v) => ComBinOp v -> VarType v -> VarType v -> TypeCheckState v (VarType v)
typecheckComBinOp Plus (ComVarType NumT) (ComVarType NumT) = return $ ComVarType NumT
typecheckComBinOp Minus (ComVarType NumT) (ComVarType NumT) = return $ ComVarType NumT
typecheckComBinOp Times (ComVarType NumT) (ComVarType NumT) = return $ ComVarType NumT
typecheckComBinOp Divide (ComVarType NumT) (ComVarType NumT) = return $ ComVarType NumT
typecheckComBinOp Xor (ComVarType NumT) (ComVarType NumT) = return $ ComVarType NumT
typecheckComBinOp Greater (ComVarType NumT) (ComVarType NumT) = return $ ComVarType NumT
typecheckComBinOp Lesser (ComVarType NumT) (ComVarType NumT) = return $ ComVarType NumT
typecheckComBinOp And (ComVarType NumT) (ComVarType NumT) = return $ ComVarType NumT
typecheckComBinOp Or (ComVarType NumT) (ComVarType NumT) = return $ ComVarType NumT
typecheckComBinOp op@Equal t1 t2 | t1 == t2  = return $ ComVarType NumT
                              | otherwise = addError (ComTypeError $ IncomparableTypes (ComBinOp op) t1 t2) >> return (ComVarType NumT)
typecheckComBinOp op@NotEqual t1 t2 | t1 == t2  = return $ ComVarType NumT
                              | otherwise = addError (ComTypeError $ IncomparableTypes (ComBinOp op) t1 t2) >> return (ComVarType NumT)
typecheckComBinOp op t1 t2 = addError (ComTypeError $ UnexpectedBinOpType (ComBinOp op) (ComVarType NumT,ComVarType NumT) (ComVarType NumT) (t1,t2)) >> return (ComVarType NumT)

typecheckComUnOp :: (VariantTypeChecker v) => ComUnOp v -> VarType v -> TypeCheckState v (VarType v)
-- Size
typecheckComUnOp Size (ComVarType (ListT _)) = return $ ComVarType NumT
typecheckComUnOp Size (ComVarType (QueueT _)) = return $ ComVarType NumT
typecheckComUnOp op@Size t = addError (ComTypeError $ UnexpectedUnOpType (ComUnOp op) (ComVarType $ ListT t) (ComVarType NumT) t) >> return (ComVarType NumT)
-- Head
typecheckComUnOp Head (ComVarType (ListT t)) = return t
typecheckComUnOp op@Head t = addError (ComTypeError $ UnexpectedUnOpType (ComUnOp op) (ComVarType $ ListT t) t t) >> return t
-- Last
typecheckComUnOp Last (ComVarType (ListT t)) = return t
typecheckComUnOp op@Last t = addError (ComTypeError $ UnexpectedUnOpType (ComUnOp op) (ComVarType $ ListT t) (ComVarType NumT) t) >> return t
-- UnOpEmpty
typecheckComUnOp UnOpEmpty _ = return (ComVarType NumT)


typecheckComValue :: (VariantTypeChecker v) => ComValue v -> TypeCheckState v (VarType v)
typecheckComValue (RefVal n) = typeof n
typecheckComValue (IntVal _) = return $ ComVarType NumT
typecheckComValue (FracVal _ _) = return $ ComVarType NumT
typecheckComValue (ListLit []) = return . ComVarType . ListT $ ComVarType Unknown
typecheckComValue (ListLit (e:_)) = ComVarType . ListT <$> typecheckExpr e
  
typecheckComValue _ = return $ ComVarType Unknown
-- TODO: add refval and procrefval

-- Compare types
compareTypes :: (TypedVariant v) => VarType v -> VarType v -> Maybe (VarType v)
compareTypes (ComVarType t1) (ComVarType t2) = compareComTypes t1 t2
compareTypes (VarVarType t1) t2 = compareVarTypes t1 t2
compareTypes t1 (VarVarType t2) = compareVarTypes t2 t1

compareComTypes :: (TypedVariant v) => ComVarType v -> ComVarType v -> Maybe (VarType v)
compareComTypes (ListT t1) (ListT t2) = ComVarType . ListT <$> compareTypes t1 t2
compareComTypes (QueueT t1) (QueueT t2) = ComVarType . QueueT <$> compareTypes t1 t2
compareComTypes NumT NumT = Just $ ComVarType NumT
compareComTypes (FunT ts1) (FunT ts2)
  | ts1 == ts2 = Just . ComVarType $ FunT ts1
  | otherwise  = Nothing
compareComTypes Unknown t  = Just $ ComVarType t
compareComTypes t Unknown  = compareComTypes Unknown t
compareComTypes _ _ = Nothing

-- Procedure type lookup
typeofProc :: (VariantTypeChecker v) => String -> TypeCheckState v (VarType v)
typeofProc nm = lookupProc nm >>= \case
  Just (ComProcedure p) -> typeofComProc p
  Just (VarProcedure p) -> typeofVarProc p
  Nothing -> return $ ComVarType Unknown

typeofComProc :: (VariantTypeChecker v) => ComProcedure v -> TypeCheckState v (VarType v)
typeofComProc f = return . ComVarType . FunT $ map snd (funcparams f)

-- Typecheck args
typecheckArgs :: (VariantTypeChecker v) => [VarType v] -> Args -> TypeCheckState v ()
typecheckArgs [] [] = return ()
typecheckArgs (t:ps) (a:as) = do
  let a' = (Var (-1,-1) a [])
  ta <- typeof a'
  case compareTypes t ta of
    Nothing -> addError (ComTypeError $ UnexpectedType a' t ta) >> typecheckArgs ps as
    _       -> typecheckArgs ps as

-- Helpers
registerType :: (VariantTypeChecker v) => VarType v -> String -> State (TypeTable v, ProcedureTable v) ()
registerType t n = do
  (m,pt) <- get
  case gettype n m of
    Nothing -> put (settype n t m,pt)
    Just t' -> return ()

addError :: (VariantTypeChecker v) => TypeError v -> TypeCheckState v ()
addError e = get >>= \(es,t) -> put (e:es,t)

typeof :: (VariantTypeChecker v) => Var v -> TypeCheckState v (VarType v)
typeof (Var _ ('@':n) []) = typeofProc n
typeof n@(Var _ ('@':_) _) = addError (ComTypeError $ TooManyIndices n) >> return (ComVarType Unknown)
typeof m@(Var _ n ids) = do
  tsym <- getTypes
  case gettype n tsym of
    Nothing -> return $ ComVarType Unknown
    Just t  -> case getIndexedType t ids of
                Nothing -> addError (ComTypeError $ TooManyIndices m) >> return (ComVarType Unknown)
                Just t' -> return t'

getIndexedType :: (VariantTypeChecker v) => VarType v -> [Expr v] -> Maybe (VarType v)
getIndexedType t [] = Just t
getIndexedType (ComVarType (ListT t)) (_:ids) = getIndexedType t ids
getIndexedType (ComVarType (QueueT t)) (_:ids) = getIndexedType t ids
getIndexedType t _  = Nothing

getTypes :: (VariantTypeChecker v) => TypeCheckState v (TypeTable v)
getTypes = gets snd
