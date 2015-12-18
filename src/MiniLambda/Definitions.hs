{-# LANGUAGE OverloadedStrings #-}

module MiniLambda.Definitions where

import MiniLambda
import Data.Map (fromList)

omega = lambda "x" <.> "x" @@ "x"

cons = lambda "x" <.> lambda "y" <.> lambda "f" <.> "f" @@ "x" @@ "y"
car = lambda "t" <.> "t" @@ (lambda "x" <.> lambda "_" <.> "x")
cdr = lambda "t" <.> "t" @@ (lambda "_" <.> lambda "y" <.> "y")

zero = lambda "f" <.> lambda "x" <.> "x"
suc = lambda "n" <.> lambda "f" <.> lambda "x" <.> "f" @@ ("n" @@ "f" @@ "x")

twice = lambda "f" <.> lambda "x" <.> "f" @@ ("f" @@ "x")

i = lambda "x" <.> "x"
k = lambda "x" <.> lambda "y" <.> "x"
s = lambda "f" <.> lambda "g" <.> lambda "x" <.> "f" @@ "x" @@ ("g" @@ "x")


prelude = fromList [("omega", omega)
                  , ("cons", cons)
                  , ("car", car)
                  , ("cdr", cdr)
                  , ("0", zero)
                  , ("zero", zero)
                  , ("suc", suc)
                  , ("twice", twice)
                  , ("S", s)
                  , ("id", i)
                  , ("I", i)
                  , ("const", k)
                  , ("K", k)]

