module Parser (Parser.parse) where

import Text.Parsec as P
import Text.Parsec (many, many1, (<|>))
import Text.Parsec.Char (string, char, oneOf, letter, alphaNum)
import Text.Parsec.String (Parser)

import qualified Coroutine.Stackless.Parser as CSP
import qualified Actor.Parser as AP

import Variants
import Error

parse :: String -> String -> Either VariantError Variant
parse nm s = case parseVariantType s of
               Nothing -> Left . InternalError $ NoVariantFound
               Just ("coroutine", s') -> parseCoroutine nm s'
               Just ("actor", s') -> parseActor nm s'
               Just _ -> Left . InternalError $ UnknownVariant


-- Coroutine
parseCoroutine :: String -> String -> Either VariantError Variant
parseCoroutine nm s = case CSP.parse nm s of
                        Left e -> Left . CoroutineError . ParseError $ e
                        Right ast -> Right . CoroutineVariant $ ast
-- Actor
parseActor :: String -> String -> Either VariantError Variant
parseActor nm s = case AP.parse nm s of
                        Left e -> Left . ActorError . ParseError $ e
                        Right ast -> Right . ActorVariant $ ast

parseVariantType :: String -> Maybe (String, String)
parseVariantType s = case P.parse ((\a b -> (a,b)) <$> parseType <*> P.getInput) "" s of
                         Left e -> Nothing
                         Right v -> Just v
  where parseType = char '@' *> parseName <* many (oneOf " ") <* P.newline
        parseName = (++) <$> many1 letter <*> many (alphaNum <|> oneOf "_-'")

