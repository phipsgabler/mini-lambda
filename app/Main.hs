module Main where

import MiniLambda
import MiniLambda.Parser
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  input <- dropWhile (==' ') <$> getLine
  handle input
  main


handle :: String -> IO ()
handle input = putStrLn $ either show eval (parseExpression input)
  where eval = show . normalizeWith prelude
