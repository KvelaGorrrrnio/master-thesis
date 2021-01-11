{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Actor.Parser (module Actor.Parser, module Common.Classes) where

import Common.Classes
import Common.Parser
import Actor.Error
import Actor.Types

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Common.Queue as Queue
import qualified Common.Zipper as Zipper
import qualified Control.Monad.Except as Except (throwError)

import Text.Parsec (try, option, optionMaybe, many, many1, oneOf, spaces, eof, (<|>))
import Text.Parsec.Char (string, alphaNum, letter)
import Text.Parsec.Indent (withBlock, indented)

instance VariantParser Actor where
  parse name input  = iparse name (spaces *> parseProgram name <* spaces <* eof) input
  parseProgram nm   = do
    globaltypes <- parseGlobals
    procs <- Map.fromList <$> (parseProcedures >>= combineMailpoints >>= hasMain)
    return $ Program nm initialBody [] procs Map.empty globaltypes Map.empty
    where initialBody = VarProcedure $ Actor "" "" (-1) "" [] Map.empty Map.empty Map.empty
          parseGlobals = Map.fromList <$> many parseGlobal
          parseGlobal  = do
            string "global" <* ws1
            t <- parseType <* ws1
            n <- parseVarname <* endline
            return (n,t)
          hasMain ps = case length (filter (("main"==) . fst) ps) of
                               n | n == 0     -> Except.throwError . VarParseError $ NoMainFound
                                 | otherwise -> return ps

          combineMailpoints :: [(String,Procedure Actor)] -> Parser Actor [(String,Procedure Actor)]
          combineMailpoints procs = do
            -- Get all functions
            let funcs = filter (isFunction.snd) procs
            -- Get all actors
            let actors = filter (isActor.snd) procs
            -- Group actors and combine
            let mailpoints = List.groupBy (\a b -> fst a == fst b) $ List.sortOn fst actors
            mailpoints' <- mapM mergeMailpoints mailpoints
            return $ funcs ++ mailpoints'
              where isFunction (ComProcedure Function {}) = True
                    isFunction _ = False
                    isActor (VarProcedure Actor {}) = True
                    isActor _ = False
                    mergeMailpoints nameactors = do
                      let (name:_,actors) = unzip nameactors
                      mps <- checkInitPoints name $ concatMap (\(VarProcedure a) -> Map.toList $ mailpoints a) actors 
                      return (name, VarProcedure $ Actor name "" (-1) "" [] (Map.fromList mps) Map.empty Map.empty)
                    checkInitPoints "main" mps =
                      case length (filter (("init"==) . fst) mps) of
                         n | n > 1     -> Except.throwError . VarParseError $ MultipleMainInits
                         n | n == 0     -> Except.throwError . VarParseError $ NoMainInitFound
                           | otherwise -> return mps
                    checkInitPoints _ mps = return mps

  parseVarProcedure = withBlock combineMailpoint parseHead (parseStmt <* endline) <* string "}" <* endline
    where parseHead :: Parser Actor (String,String,Stmts Actor -> Mailpoint)
          parseHead = string "actor" *> ws1 *> parseMailpoint <* string "{" <* endline
          parseMailpoint :: Parser Actor (String, String, Stmts Actor -> Mailpoint)
          parseMailpoint = do
            name <- parseFunname
            string "."
            mailpoint <- parseFunname <* ws
            params <- parseParams <* ws
            sender <- optionMaybe (string "from" *> ws1 *> parseVarname)
            return (name, mailpoint, Mailpoint mailpoint params sender)
          combineMailpoint :: (String, String, Stmts Actor -> Mailpoint) -> [Stmt Actor] -> (String, Procedure Actor)
          combineMailpoint (name,mp,mailpoint) stmts = (name, VarProcedure $ Actor name "" (-1) "" [] (Map.fromList [(mp, mailpoint $ Zipper.fromList stmts)])  Map.empty Map.empty)

  parseVarStmt      = VarStmt <$> (try parseSpawn <|> parseDespawn <|> parseSend <|> parseUnsend)
    where parseSpawn = Spawn <$> getPos <*> (string "spawn" *> ws1 *> parseLabel) <*> (parseFunname <* ws)
          parseDespawn = Despawn <$> getPos <*> (string "despawn" *> ws1 *> parseLabel) <*> (parseFunname <* ws)
          parseSend = Send <$> getPos <*> (string "send" *> ws1 *> parseActorRef <* ws) <*> (string "." *> parseVarname) <*> parseArgs 
          parseUnsend = Unsend <$> getPos <*> (string "unsend" *> ws1 *> parseActorRef <* ws) <*> (string "." *> parseVarname) <*> parseArgs 
  parseVarValue     = impossible >> undefined
 -- parseVarValue     = VarValue <$> (parseSelf)
 --   where parseSelf = ActorSelf <$> getPos
  parseVarUnOp      = impossible >> undefined
  parseVarBinOp _   = impossible >> undefined
  parseVarType      = VarVarType ActorT <$ string "actor"

-- Helpeers
parseLabel :: Parser Actor String
parseLabel = (++) <$> ((++) <$> option "" (string "$") <*> many1 letter) <*> many (alphaNum <|> oneOf "_'") <* ws

parseActorRef :: Parser Actor (Either String String)
parseActorRef = try (Right <$> string "self") <|> (Left <$> parseFunname)
