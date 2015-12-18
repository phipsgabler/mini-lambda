{-# LANGUAGE OverloadedStrings #-}

module MiniLambda.Definitions where

import MiniLambda
import Data.Map (fromList)

omega = lambda "x" <.> "x" @@ "x"

cons = lambda "x" <.> lambda "y" <.> lambda "f" <.> "f" @@ "x" @@ "y"
car = lambda "t" <.> "t" @@ (lambda "x" <.> lambda "_" <.> "x")
cdr = lambda "t" <.> "t" @@ (lambda "_" <.> lambda "y" <.> "y")

zero = lambda "f" <.> lambda "x" <.> "x"
succ = lambda "n" <.> lambda "f" <.> lambda "x" <.> "f" @@ ("n" @@ "f" @@ "x")


prelude = fromList [("omega", omega)
                  , ("cons", cons)
                  , ("car", car)
                  , ("cdr", cdr)]
