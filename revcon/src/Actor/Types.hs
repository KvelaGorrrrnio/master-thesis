{-# LANGUAGE TypeFamilies, StandaloneDeriving, FlexibleInstances #-}
module Actor.Types (module Actor.Types, module Common.Types) where

import Common.Types
import Common.Classes
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Common.Queue as Queue

-- Program
data Actor = ActorVariantReference deriving Show
data instance Program Actor =
    Program { progname    :: String
            , progbody    :: Procedure Actor
            , progstack   :: [Procedure Actor]
            , procedures  :: ProcedureTable Actor
            , actors      :: Map.Map ActorId (VarProcedure Actor)
            , globaltypes :: TypeTable Actor
            , globalstore :: SymbolTable Actor
            }

instance Show (Program Actor) where
  show p = progname p ++ ":" ++ show (progbody p) ++ "\n\n[procedures]\n" ++ List.intercalate "\n" (map (printProcedure . snd) (Map.toList $ procedures p)) ++ "\n\n[globals]\n" ++ printGlobals
    where printProcedure (ComProcedure p) = funcname p
          printProcedure (VarProcedure p) = List.intercalate "\n" (map (printMailpoint (actoname p) . snd) (Map.toList $ mailpoints p))
          printMailpoint n (Mailpoint m ps _ _) = n ++ "." ++ m ++ "(" ++ List.intercalate ", " (map fst ps) ++ ")"
          printGlobals = List.intercalate "\n" (map (\(k,v) -> k ++ ": " ++ show v) (Map.toList $ globalstore p))

-- Message
data Message = Message ActorId ActorId MessageContent
             deriving Show
data MessageContent = MessageContent String [Value Actor]
                    deriving Show
-- Actor
type ActorId = Int
data Mailpoint = Mailpoint { mailpointName   :: String
                           , mailpointParams :: Params Actor
                           , mailpointSender :: Maybe String
                           , mailpointBody   :: Stmts Actor
                           }
               deriving Show
data instance VarProcedure Actor = Actor { actoname   :: String
                                         , actolabel  :: String
                                         , actorid    :: ActorId
                                         , mailpoint  :: String
                                         , actobody   :: [Stmts Actor]
                                         , mailpoints :: Map.Map String Mailpoint 
                                         , actostore  :: SymbolTable Actor
                                         , actotypes  :: TypeTable Actor
                                         }

instance (ProcedureInfo Actor) where
  name (VarProcedure c) = actoname c
  name (ComProcedure f) = funcname f
  store (VarProcedure c) = actostore c
  store (ComProcedure f) = funcstore f
  types (VarProcedure c) = actotypes c
  types (ComProcedure f) = functypes f
  params (VarProcedure c) = [] -- TODO: possibly alter
  params (ComProcedure f) = funcparams f
  body (VarProcedure c) = actobody c
  body (ComProcedure f) = funcbody f

deriving instance Show (ComProcedure Actor)
instance Show (Procedure Actor) where
  show (ComProcedure p) = show p
  show (VarProcedure p) = show p

instance Show (VarProcedure Actor) where
  show a = actoname a ++ if actolabel a /= "" then" (" ++ actoname a ++ ")" else "" ++ "\n[store]\n" ++ List.intercalate "\n" (map (\(k,v)-> "  " ++ k ++ ": " ++ show v) (Map.toList $ actostore a)) ++ "\n\n[stack]" ++ List.intercalate "\n------\n" (map show (actobody a))

-- Statements
data instance VarStmt Actor = Spawn Pos String String
                            | Despawn Pos String String
                            | Send Pos (Either String String) String Args
                            | Unsend Pos (Either String String) String Args
                            | Deliver Pos String
                            | Undeliver Pos String

instance Show (VarStmt Actor) where
  show (Spawn _ l f) = "spawn " ++ l ++ " " ++ f
  show (Despawn _ l f) = "despawn " ++ l ++ " " ++ f
  show (Send _ a m as) = "send " ++ show a ++ "." ++ m ++ "(" ++ List.intercalate ", " as ++ ")"
  show (Unsend _ a m as) = "unsend " ++ show a ++ "." ++ m ++ "(" ++ List.intercalate ", " as ++ ")"
  show (Deliver _ m) = "deliver " ++ show m
  show (Undeliver _ m) = "undeliver " ++ show m

instance Show (Stmt Actor) where
  show (ComStmt s) = show s
  show (VarStmt s) = show s

-- Values
data instance VarValue Actor = ActorVal ActorId
                             | MessageVal ActorId ActorId String [Value Actor]
                             | ActoSelf
deriving instance Show (VarValue Actor)
instance Eq (VarValue Actor) where
  (ActorVal n) == (ActorVal m) = n == m
  (MessageVal f1 t1 mp1 _) == (MessageVal f2 t2 mp2 _) = f1 == f2 && t1 == t2 && mp1 == mp2
  ActoSelf     == ActoSelf   = True
  _ == _ = False
instance Show (Value Actor) where
  show (ComValue v) = show v
  show (VarValue v) = show v

-- Unary Operators
data instance VarUnOp Actor
deriving instance Show (VarUnOp Actor)
instance Show (UnOp Actor) where
  show (ComUnOp op) = show op
  show (VarUnOp op) = show op

-- Binary Operators 
data instance VarBinOp Actor
deriving instance Show (VarBinOp Actor)
instance Show (BinOp Actor) where
  show (ComBinOp op) = show op
  show (VarBinOp op) = show op

-- Variable Types
data instance VarVarType Actor = ActorT
                               | MessageT
                               deriving (Show, Eq)
