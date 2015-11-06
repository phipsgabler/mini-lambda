{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module MiniLambda.Parser
    ( parseExpression
    ) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor (second)
import MiniLambda.Parser.Internal
import MiniLambda hiding (lambda, (<.>))


-- parse to end and get result out of tuple
parseExpression = second fst . runParser expression

expression = variable
   <|> lambda
   <|> application

-- ascii alphanum + symbols, except '.', '\', '(', and ')'
letter = oneOf $ ['!'..'&'] ++ ['*'..'-'] ++ ['/'..'['] ++ [']'..'~'] 

identifier = many1 letter

variable = Var <$> identifier

lambda = parenthesized $ do
  char '\\'
  skipWhitespace
  v <- identifier
  skipWhitespace
  char '.'
  skipWhitespace
  e <- expression
  return $ Lambda v e

application = (debugWith "application") . parenthesized $ do
  e1 <- expression
  skipWhitespace
  e2 <- expression
  return $ App e1 e2

-- helpers
skipWhitespace = many $ char ' '
parenthesized p = (char '(' *> skipWhitespace) *> p <* (skipWhitespace <* char ')')
debugWith msg p = p <|> perror msg
