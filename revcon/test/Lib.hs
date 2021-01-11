{-# LANGUAGE LambdaCase #-}
module Lib 
  ( Target (..)
  , listScripts
  , getSuites
  , runSuites
  , evaluate
  ) where

import qualified System.FilePath.Glob as Glob
import qualified System.Directory as Directory (doesFileExist)
import qualified System.FilePath.Posix as FilePath (takeBaseName)
import qualified System.Process as Process (readProcessWithExitCode)
import qualified System.Exit as Exit (ExitCode (..), exitFailure)
import qualified Data.Char as Char (isSpace)
import Control.Monad (filterM)

import qualified Parser
import qualified Static
import qualified Error

data Target = All
            | Coroutine
            | Actor
            deriving Show

data Suite = Suite { name   :: String
                   , input  :: String
                   , cases  :: [SuiteCase]
                   , failed :: Bool
                   } deriving Show

data SuiteCase = SuiteCase Mode String (Maybe (String,String)) deriving Show
data Mode = Run | Inverse | Runverse
instance Show Mode where
  show Run = "run"
  show Inverse = "inverse"
  show Runverse = "runverse"

testdir = "test/suite"

listFiles :: String -> [String] -> IO [String]
listFiles dir patterns = fmap FilePath.takeBaseName . (!!0) <$> Glob.globDir (map Glob.compile patterns) dir

listScripts :: Target -> IO [String]
listScripts All       = concat <$> mapM listScripts [Coroutine,Actor]
listScripts Coroutine = listFiles testdir ["*.coroutine.revcon"]
listScripts Actor     = listFiles testdir ["*.actor.revcon"]

getSuites :: [String] -> [Suite]
getSuites = map getSuite

getSuite :: String -> Suite
getSuite n = Suite n (testdir ++ "/" ++ n ++ ".revcon") [ SuiteCase Run      (testdir ++ "/" ++ n ++ ".out")      Nothing
                                                        , SuiteCase Inverse  (testdir ++ "/" ++ n ++ ".inverse")  Nothing
                                                        , SuiteCase Runverse (testdir ++ "/" ++ n ++ ".runverse") Nothing
                                                        ] False


runSuites :: [Suite] -> IO [Suite]
runSuites = mapM runSuite

runSuite :: Suite -> IO Suite
runSuite suite = do
  putStr (colorScr (name suite ++ ": "))
  cs <- runSuiteCases (input suite) $ cases suite
  let failed' = any (\(SuiteCase _ _ o) -> o /= Nothing) cs
  if failed' then putStrLn (colorErr "Failed") >> mapM_ printSuiteCase cs
             else putStrLn (colorSuc "Success")
  
  return suite { failed = failed' }

printSuiteCase :: SuiteCase -> IO ()
printSuiteCase (SuiteCase mode _ Nothing)      = putStrLn $ "  " ++ show mode ++ ": " ++ colorSuc "Success"
printSuiteCase (SuiteCase mode _ (Just (t,f))) = putStrLn $ "  " ++ show mode ++ ": " ++ colorErr "Failed" ++ "\nExpected:\n" ++ colorExp t ++ "\n, but got:\n" ++ colorErr f ++ "\n"

runSuiteCases :: String -> [SuiteCase] -> IO [SuiteCase]
runSuiteCases path cases = filterM hasOutFile cases >>= mapM (runSuiteCase path)
  where hasOutFile (SuiteCase _ path _) = Directory.doesFileExist path

runSuiteCase :: String -> SuiteCase -> IO SuiteCase
runSuiteCase path (SuiteCase mode outpath _) = do
  -- Load outfile
  out <- Directory.doesFileExist outpath >>= \case
    True -> readFile outpath
    False -> return ""
  -- Execute program
  (exitcode,stdout,stderr) <- Process.readProcessWithExitCode "stack" ["exec", "revcon", show mode, "--", path] ""
  -- Get ouput
  let result = case exitcode of
       Exit.ExitSuccess   -> stdout
       Exit.ExitFailure _ -> stderr
  -- Compare
  if trim result == trim out
     then return (SuiteCase mode outpath Nothing)
     else return (SuiteCase mode outpath $ Just (trim out, trim result))


evaluate :: [Suite] -> IO ()
evaluate ss =
  let successful = filter (not . failed) ss in
      if length successful == length ss
         then putStrLn (colorSuc "\n\nAll tests passed.")
         else putStrLn (colorScr "\n\nNot all tests passed." ++ colorErr ("\n  Failed: " ++ show (length ss - length successful) ++ "/" ++ show (length ss) ++ ".")) >> Exit.exitFailure 

-- Helpers
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile Char.isSpace

-- Color 
colorExp s = "\x1b[36m" ++ s ++ "\x1b[0m"
colorErr s = "\x1b[31m" ++ s ++ "\x1b[0m"
colorSuc s = "\x1b[32m" ++ s ++ "\x1b[0m"
colorScr s = "\x1b[35m" ++ s ++ "\x1b[0m"
