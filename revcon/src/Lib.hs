{-# LANGUAGE LambdaCase #-}
module Lib
    ( someFunc
    --, someOtherFunc
    ) where

import Parser
import Static
import Interpreter
import Inverter
import System.Environment (getArgs)
import Debug.Trace

import Variants
import Coroutine.Stackless.Types


someFunc :: IO ()
someFunc = getArgs >>= \case
  []  -> putStrLn "No mode given."
  ["run"] -> putStrLn "No file given."
  ["inverse"] -> putStrLn "No file given."
  ["runverse"] -> putStrLn "No file given."
  "run":f:_ -> readFile f >>= printProgram  . interpret . typecheck . parse f
  "inverse":f:_ -> readFile f >>= printProgram  . interpret . invert . typecheck . parse f
  "runverse":f:_ -> readFile f >>= printProgram . interpret . invert . interpret . typecheck . parse f
  _ -> putStrLn "Unknown mode."

printProgram (Left x) = print x
printProgram (Right (CoroutineVariant p)) = print p
printProgram (Right (ActorVariant p)) = print p

