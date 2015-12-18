module Main where

import MiniLambda
import MiniLambda.Parser
import MiniLambda.Definitions
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  input <- dropWhile (==' ') <$> getLine
  handle input
  main


handle :: String -> IO ()
handle "" = return ()
handle input = putStrLn $ either show eval (parseExpression input)
  where eval = show . normalizeFull prelude
