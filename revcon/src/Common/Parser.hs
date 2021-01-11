module Common.Parser (module Common.Parser) where

import Text.Parsec (try, option, many, many1, sepBy, sepBy1, endBy1, chainl1, newline, sourceLine, sourceColumn, getPosition, (<|>), runParserT)
import Text.Parsec.Char (string, char, oneOf, noneOf, letter, alphaNum, digit)
import Text.Parsec.Indent (runIndentParserT, withBlock, indented, withPos, checkIndent)

import qualified Control.Monad.Except as Except

import Common.Types
import Common.Classes
import qualified Common.Error as Error
import qualified Data.Map as Map
import qualified Common.Zipper as Zipper


-- IndentParser definitions
iparse :: (VariantParser v) => String -> Parser v (Program v) -> String -> Either (Error.ParseError v) (Program v)
iparse name parser src = case (Except.runExcept . runIndentParserT parser () name) src of
                       Left e -> Left e
                       Right (Left e) -> Left $ Error.ComParseError e
                       Right (Right p) -> Right p

-- Procedures
parseProcedures :: (VariantParser v) => Parser v [(String, Procedure v)]
parseProcedures = many1 parseProcedure

parseProcedure :: (VariantParser v) => Parser v (String, Procedure v)
parseProcedure = try parseVarProcedure <|> parseComProcedure

parseComProcedure :: (VariantParser v) => Parser v (String, Procedure v)
parseComProcedure = parseFunction

parseFunction :: (VariantParser v) => Parser v (String, Procedure v)
parseFunction = withBlock combineFunction parseFunctionHead parseFunctionBody <* string "}" <* endline
  where parseFunctionHead :: (VariantParser v) => Parser v (String, Params v)
        parseFunctionHead = tpl <$> (string "function" *> ws1 *> parseFunname <* ws) <*> parseParams <* string "{" <* endline
        parseFunctionBody :: (VariantParser v) => Parser v (Stmt v)
        parseFunctionBody = parseStmt <* endline
        combineFunction :: (VariantParser v) => (String, Params v) -> [Stmt v] -> (String, Procedure v)
        combineFunction (name,params) stmts = (name, ComProcedure $ Function name params [Zipper.fromList $ ComStmt FunctionReturn:(stmts ++ [ComStmt FunctionReturn])] Map.empty Map.empty)
        tpl :: (VariantParser v) => String -> Params v -> (String, Params v)
        tpl name params = (name,params)


-- Statements
parseStmts :: (VariantParser v) => Parser v [Stmt v]
parseStmts = indented *> withPos (endBy1 (checkIndent *> parseStmt) endline)

-- Statement
parseStmt :: (VariantParser v) => Parser v (Stmt v)
parseStmt = try parseVarStmt <|> parseComStmt

-- Common Statements
parseComStmt :: (VariantParser v) => Parser v (Stmt v)
parseComStmt = ComStmt <$> ( try parseUpdate <|> try parseSkip <|> try parseSwap <|> parseIf <|> parseLoop
                         <|> try parsePop <|> parseCall <|> try parseUncall <|> parsePush
                         <|> try parseEnqueue <|> try parseUnenqueue <|> try parseDequeue <|> try parseUndequeue
                         <|> parseLocal <|> parseDelocal)

parseSkip :: (VariantParser v) => Parser v (ComStmt v)
parseSkip = string "skip" >> return Skip

parseSwap :: (VariantParser v) => Parser v (ComStmt v)
parseSwap = Swap <$> getPos <*> (string "swap" >> ws1 >> parseVar <* ws1) <*> parseVar

parseUpdate :: (VariantParser v) => Parser v (ComStmt v)
parseUpdate = Update <$> getPos <*> (parseVar <* ws) <*> parseBinOp (-1) <* char '=' <* ws <*> parseExpr

parseIf :: (VariantParser v) => Parser v (ComStmt v)
parseIf = withPos (If <$> getPos <*> parseCond <*> parseThen <*> parseElse <*> parseAss <*> return True)
  where parseCond = string "if" *> ws1 *> parseExpr <* _endline
        parseThen = Zipper.fromList <$> (string "then" *> _endline *> (try parseStmts <|> ((:[]) <$> (parseStmt <* _endline))))
        parseElse = Zipper.fromList <$> (string "else" *> _endline *> (try parseStmts <|> ((:[]) <$> (parseStmt <* _endline))))
        parseAss  = string "fi" *> ws1 *> parseExpr

parseLoop :: (VariantParser v) => Parser v (ComStmt v)
parseLoop = withPos (Loop <$> getPos <*> parseCond <*> option Zipper.empty parseThen <*> option Zipper.empty parseElse <*> parseAss <*> return True)
  where parseCond = string "from" *> ws1 *> parseExpr <* _endline
        parseThen = Zipper.fromList <$> (string "do" *> _endline *> (try parseStmts <|> ((:[]) <$> (parseStmt <* _endline))))
        parseElse = Zipper.fromList <$> (string "loop" *> _endline *> (try parseStmts <|> ((:[]) <$> (parseStmt <* _endline))))
        parseAss  = string "until" *> ws1 *> parseExpr

parsePop :: (VariantParser v) => Parser v (ComStmt v)
parsePop = Pop <$> getPos <*> (string "pop" *> ws1 *> parseVar <* ws1) <*> parseVar

parsePush :: (VariantParser v) => Parser v (ComStmt v)
parsePush = Push <$> getPos <*> (string "push" *> ws1 *> parseVar <* ws1) <*> parseVar

parseEnqueue :: (VariantParser v) => Parser v (ComStmt v)
parseEnqueue = Enqueue <$> getPos <*> (string "enqueue" *> ws1 *> parseVar <* ws1) <*> parseVar

parseUnenqueue :: (VariantParser v) => Parser v (ComStmt v)
parseUnenqueue = Unenqueue <$> getPos <*> (string "unenqueue" *> ws1 *> parseVar <* ws1) <*> parseVar

parseDequeue :: (VariantParser v) => Parser v (ComStmt v)
parseDequeue = Dequeue <$> getPos <*> (string "dequeue" *> ws1 *> parseVar <* ws1) <*> parseVar

parseUndequeue :: (VariantParser v) => Parser v (ComStmt v)
parseUndequeue = Undequeue <$> getPos <*> (string "undequeue" *> ws1 *> parseVar <* ws1) <*> parseVar

parseCall :: (VariantParser v) => Parser v (ComStmt v)
parseCall = Call <$> getPos <*> (string "call" *> ws1 *> parseVar <* ws) <*> parseArgs

parseUncall :: (VariantParser v) => Parser v (ComStmt v)
parseUncall = Uncall <$> getPos <*> (string "uncall" *> ws1 *> parseVar <* ws) <*> parseArgs

parseLocal :: (VariantParser v) => Parser v (ComStmt v)
parseLocal = Local <$> getPos <*> (string "local" *> ws1 *> parseType <* ws) <*> (parseVarname `sepBy1` (ws <* string "," <* ws)) <*> option (Value . ComValue $ Empty) parseExpr

parseDelocal :: (VariantParser v) => Parser v (ComStmt v)
parseDelocal = Delocal <$> getPos <*> (string "delocal" *> ws1 *> parseType <* ws) <*> (parseVarname `sepBy1` (ws <* string "," <* ws)) <*> option (Value . ComValue $ Empty) parseExpr

parseType :: (VariantParser v) => Parser v (VarType v)
parseType = try parseComVarType <|> parseVarType

parseComVarType :: (VariantParser v) => Parser v (VarType v)
parseComVarType = ComVarType <$> ((NumT <$ string "int") 
                                 <|> try (NumT <$ string "frac")
                                 <|> (FunT <$> (string "function" *> ws *> parseTypeParams))
                                 <|> (ListT <$> (string "list" *> ws *> parseType))
                                 <|> (QueueT <$> (string "queue" *> ws *> parseType))
                                 )

parseTypeParams :: (VariantParser v) => Parser v [VarType v]
parseTypeParams = parens $ (parseType <* ws) `sepBy` (char ',' <* ws)

-- Expressions
parseExpr :: (VariantParser v) => Parser v (Expr v)
parseExpr = parseExprBranch 0 100

parseExprBranch :: (VariantParser v) => Int -> Int -> Parser v (Expr v)
parseExprBranch n m
  | n >= m    = parseExprLeaf <* ws
  | otherwise = parseExprBranch (n+1) m `chainl1` (BinOp <$> parseBinOp n <* ws)

parseExprLeaf :: (VariantParser v) => Parser v (Expr v)
parseExprLeaf = (UnOp <$> parseUnOp <*> parseExprLeaf) <|> (Value <$> parseValue <|> parens parseExpr)

-- Values
parseValue :: (VariantParser v) => Parser v (Value v)
parseValue = try parseVarValue <|> parseComValue

-- Common Values
parseComValue :: (VariantParser v) => Parser v (Value v)
parseComValue = ComValue <$> (parseListLit <|> parseIntVal <|> parseRefVal)
  where parseIntVal  = IntVal . read <$> many1 digit
        parseRefVal  = RefVal <$> parseVar
        parseListLit = ListLit <$> (char '[' *> ws *> parseExpr `sepBy` (char ',' <* ws) <* char ']' <* ws)

-- Unary Operators
parseUnOp :: (VariantParser v) => Parser v (UnOp v)
parseUnOp = try parseComUnOp <|> try parseVarUnOp

-- Common Unary Operators
parseComUnOp :: (VariantParser v) => Parser v (UnOp v)
parseComUnOp = ComUnOp . comUnOp <$> (string "size" <|> string "head" <|> string "last" <|> string "empty")<* ws1

-- Binary Operators
parseBinOp :: (VariantParser v) => Int -> Parser v (BinOp v)
parseBinOp n = try (parseVarBinOp n) <|> parseComBinOp n

comUnOp :: (VariantParser v) => String -> ComUnOp v
comUnOp "size"  = Size
comUnOp "head"  = Head
comUnOp "last"  = Last
comUnOp "empty" = UnOpEmpty

-- Common Binary Operators
parseComBinOp :: (VariantParser v) => Int -> Parser v (BinOp v)
parseComBinOp (-1) = ComBinOp . comBinOp . (:[]) <$> oneOf "+-*/^"
parseComBinOp 10   = ComBinOp . comBinOp . (:[]) <$> oneOf "+-"
parseComBinOp 20   = ComBinOp . comBinOp . (:[]) <$> oneOf "*/"
parseComBinOp 30   = ComBinOp . comBinOp . (:[]) <$> oneOf "^"
parseComBinOp 40   = ComBinOp . comBinOp  <$> string "or"
parseComBinOp 50   = ComBinOp . comBinOp  <$> string "and"
parseComBinOp 80   = ComBinOp . comBinOp  <$> (string "==" <|> string "!=")
parseComBinOp 90   = ComBinOp . comBinOp . (:[]) <$> oneOf "<>"
parseComBinOp _    = impossible >> undefined

comBinOp :: (VariantParser v) => String -> ComBinOp v
comBinOp "+"   = Plus
comBinOp "-"   = Minus
comBinOp "*"   = Times
comBinOp "/"   = Divide
comBinOp "^"   = Xor
comBinOp ">"   = Greater
comBinOp "<"   = Lesser
comBinOp "=="  = Equal
comBinOp "!="  = NotEqual
comBinOp "and" = And
comBinOp "or"  = Or

-- Helpers
parseParams :: (VariantParser v) => Parser v (Params v)
parseParams = parens $ ((\t p->(p,t)) <$> parseType <* ws <*> parseVarname <* ws) `sepBy` (ws *> char ',' <* ws)

parseArgs :: (VariantParser v) => Parser v Args
parseArgs = parens $ (parseVarname <* ws) `sepBy` (ws *> char ',' <* ws)

parseFunname :: (VariantParser v) => Parser v String
parseFunname = (++) <$> option "" (string "@") <*> ((++) <$> ((++) <$> option "" (string "$") <*> many1 letter) <*> many (alphaNum <|> oneOf "_'"))

ws :: (VariantParser v) => Parser v ()
ws = many (char ' ') >> return ()

ws1 :: (VariantParser v) => Parser v ()
ws1 = many1 (char ' ') >> return ()

wsn :: (VariantParser v) => Parser v ()
wsn = ws <* (newline *> ws <|> cmt)

endline :: (VariantParser v) => Parser v ()
endline = wsn *> ws <* many (newline *> ws <|> cmt)

_endline :: (VariantParser v) => Parser v ()
_endline =  ws <* many (newline *> ws)

cmt :: (VariantParser v) => Parser v ()
cmt = char '#' *> many (noneOf "\n") *> wsn

parens :: (VariantParser v) => Parser v a -> Parser v a 
parens p = char '(' *> ws *> p <* char ')' <* ws

parseVar :: (VariantParser v) => Parser v (Var v)
parseVar = Var <$> getPos <*> (try parseFunname <|> parseVarname) <*> many (string "[" *> parseExpr <*string "]")

parseVarname :: (VariantParser v) => Parser v String
parseVarname = (++) <$> ((++) <$> option "" (string "$") <*> many1 letter) <*> many (alphaNum <|> oneOf "_'") <* ws

impossible :: (VariantParser v) => Parser v ()
impossible = () <$ char '\0'

getPos :: (VariantParser v) => Parser v Pos
getPos = do
  pos <- getPosition
  return (sourceLine pos + 1, sourceColumn pos)
