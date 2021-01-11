{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Coroutine.Stackless.Parser (module Coroutine.Stackless.Parser, module Common.Classes) where

import Coroutine.Stackless.Types hiding (ParseError)
import Coroutine.Stackless.Error
import Common.Classes
import Common.Parser

import qualified Data.Map as Map
import qualified Common.Zipper as Zipper
import qualified Control.Monad.Except as Except (throwError)

import Text.Parsec (try, option, many, many1, sepBy, oneOf, spaces, eof, (<|>))
import Text.Parsec.Char (string, char, letter, alphaNum)
import Text.Parsec.Indent (withBlock, indented)

-- Main part
instance VariantParser Coroutine where
  parse name input = iparse name (spaces *> parseProgram name <* spaces <* eof) input

  -- Program
  parseProgram nm = Program nm initialBody Map.empty [] . Map.fromList <$> (parseProcedures >>= multipleMains)
    where initialBody = VarProcedure $ Coroutine "" (-1) "" [] [] [Zipper.empty] Map.empty Map.empty
          multipleMains ps = case length (filter (("main"==) . fst) ps) of
                               n | n > 1     -> Except.throwError . VarParseError $ MultipleMains
                               n | n == 0     -> Except.throwError . VarParseError $ NoMainFound
                                 | otherwise -> return ps

  -- Procedures (Coroutine)
  parseVarProcedure = withBlock combineCoroutine parseHead parseBody <* string "}" <* endline
    where parseHead :: Parser Coroutine (String,Params Coroutine,Params Coroutine)
          parseHead = tpl <$> (string "coroutine" *> ws1 *> parseFunname <* ws) <*> parseParams <*> option [] (string "with" *> ws *> parseParams) <* string "{" <* endline
          parseBody :: Parser Coroutine (Stmt Coroutine)
          parseBody = parseStmt <* endline
          combineCoroutine :: (String,Params Coroutine,Params Coroutine) -> [Stmt Coroutine] -> (String, Procedure Coroutine)
          combineCoroutine (name,params,sparams) stmts = (name, VarProcedure $ Coroutine name (-1) "" params sparams [Zipper.fromList (VarStmt (Yield (-1,-1) "begin"):stmts++[VarStmt $ Yield (-1,-1) "end"])] Map.empty Map.empty)
          tpl :: a -> b -> c -> (a,b,c)
          tpl a b c = (a,b,c)
  
  -- Statements
  parseVarStmt = VarStmt <$> (parseSpawn <|> parseDespawn <|> parseResume <|> parseYield <|> try parseUnresume)
    where parseSpawn   = Spawn <$> getPos <*> (string "spawn" *> ws1 *> parseLabel) <*> (parseFunname <* ws) <*> option "begin" parseEntrypoint <*> option [] parseArgs
          parseDespawn = Despawn <$> getPos <*> (string "despawn" *> ws1 *> parseLabel) <*> (parseFunname <* ws) <*> option "end" parseEntrypoint <*> option [] parseArgs
          parseResume  = Resume <$> getPos <*> (string "resume" *> ws1 *> parseLabel) <*> parseArgs
          parseYield   = Yield <$> getPos <*> (string "yield" *> ws1 *> parseEntrypoint)
          parseUnresume  = Unresume <$> getPos <*> (string "unresume" *> ws1 *> parseLabel) <*> parseArgs

  parseVarValue   = impossible >> undefined
  parseVarUnOp    = impossible >> undefined
  parseVarBinOp _ = impossible >> undefined
  parseVarType    = VarVarType . CoroutineT <$> (string "coroutine" *> ws *> parseTypeParams)

-- Helpeers
parseLabel :: Parser Coroutine String
parseLabel = (++) <$> ((++) <$> option "" (string "$") <*> many1 letter) <*> many (alphaNum <|> oneOf "_'") <* ws

parseEntrypoint :: Parser Coroutine String
parseEntrypoint = (++) <$> ((++) <$> option "" (string "$") <*> many1 letter) <*> many (alphaNum <|> oneOf "_'") <* ws
