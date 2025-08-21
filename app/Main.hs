module Main where

import Options.Applicative
import System.Console.Haskeline
import TASDB.Core

-- Parsing -------------------------------------------------------------
data Command
  = MetricNew String MetricType
  | MetricDelete String
  | MetricList
  | EntryNew String MetricValue
  deriving (Show)

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "metric" (info metricParser (progDesc "Metric commands"))
        <> command "entry" (info entryParser (progDesc "Entry commands"))
    )

metricParser :: Parser Command
metricParser =
  hsubparser
    ( command "new" (info metricNew (progDesc "Create metric"))
        <> command "delete" (info metricDelete (progDesc "Delete metric"))
        <> command "ls" (info (pure MetricList) (progDesc "List metrics"))
    )

metricNew :: Parser Command
metricNew =
  MetricNew
    <$> argument str (metavar "NAME")
    <*> argument auto (metavar "TYPE")

metricDelete :: Parser Command
metricDelete =
  MetricDelete
    <$> argument str (metavar "NAME")

entryParser :: Parser Command
entryParser = hsubparser (command "new" (info entryNew (progDesc "Add entry")))

entryNew :: Parser Command
entryNew =
  EntryNew
    <$> argument str (metavar "METRIC")
    <*> argument auto (metavar "VALUE")

-- repl --------------------------------------------------------
repl :: DBState -> IO ()
repl st = runInputT defaultSettings (loop st)
  where
    loop s = do
      minput <- getInputLine "tasdb> "
      case minput of
        Nothing -> outputStrLn "bye."
        Just line ->
          case execParserPure Options.Applicative.defaultPrefs (info (commandParser <**> helper) idm) (words line) of
            Success cmd -> do
              let s' = runCommand s cmd
              outputStrLn $ show s'
              loop s'
            Failure failure -> do
              let (msg, _) = renderFailure failure "tasdb"
              outputStrLn msg
              loop s
            CompletionInvoked _ -> loop s

runCommand :: DBState -> Command -> DBState
runCommand (DBState tenv denv) (MetricNew n t) = DBState (addMetric tenv n t) denv
runCommand (DBState tenv denv) (MetricDelete n) = DBState (removeMetric tenv n) denv
runCommand (DBState tenv denv) (EntryNew n v) = DBState tenv (addDataPoint tenv denv n v)
runCommand st MetricList = st

main :: IO ()
main = repl (DBState [] [])
