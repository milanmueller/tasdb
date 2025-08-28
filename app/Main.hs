module Main (main) where

import Control.Monad.State
import Core
import Data.HashSet as HashSet
import Data.Time (UTCTime, getCurrentTime)
import Parser
import System.IO (hFlush, stdout)
import Text.Parsec (parse)

-- Execute commands
runParserCmd :: Cmd -> UTCTime -> DBMonad ()
runParserCmd (MCmd cmd) _ = runParserMCmd cmd
runParserCmd (VCmd cmd) t = runParserVCmd cmd t

-- Helper functions to convert types and values from parser datatypes to ast datatypes
runParserType :: Type -> MetricType
runParserType Parser.TInt = Core.TInt
runParserType Parser.TBool = Core.TBool
runParserType (Parser.TEnum variants) = Core.TEnum (fromList variants)

runParserValue :: Val -> MetricValue
runParserValue (VStr str) = VEnum str
runParserValue (Parser.VInt val) = Core.VInt val
runParserValue (Parser.VBool bool) = Core.VBool bool

runParserMCmd :: MetricCmd -> DBMonad ()
runParserMCmd (Add mname mtype) = addMetric mname (runParserType mtype)

runParserVCmd :: ValueCmd -> UTCTime -> DBMonad ()
runParserVCmd (Insert mname val) = addDataPoint mname (runParserValue val)

executeLine :: DBState -> String -> IO ()
executeLine _ ":q" = putStrLn "Bye!"
executeLine st ":show" = do
  print st
  repl st
executeLine st line = do
  currentTime <- getCurrentTime
  case parse parseCmd "" line of
    Left err -> do
      print err
      repl st
    Right cmd -> do
      let result = runStateT (runParserCmd cmd currentTime) st
      case result of
        Left dbErr -> do
          putStrLn $ "Error: " ++ show dbErr
          repl st
        Right ((), newState) -> do
          putStrLn "OK"
          repl newState

repl :: DBState -> IO ()
repl st = do
  putStr "> "
  hFlush stdout
  line <- getLine
  executeLine st line

main :: IO ()
main = do
  putStrLn "Welcome to TASDB. Enter commands, :show to show current state or :q to exit."
  repl emptyDBState
