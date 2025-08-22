module Main (main) where

import Parser (parse, parseCmd)
import System.IO (hFlush, stdout)

-- REPL
repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  line <- getLine
  case parse parseCmd "" line of
    Left err -> print err
    Right cmd -> print cmd
  repl

main :: IO ()
main = repl
