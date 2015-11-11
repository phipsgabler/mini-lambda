{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

module MiniLambda
    ( Expr(..),
      normalize,
      normalizeWith,
      prelude,
      lambda,
      (<.>),
      (@@)
    ) where

import Data.String
import qualified Data.Map.Strict as M

data Expr = Var { name :: String }
          | App Expr Expr
          | Lambda { arg :: String, expr :: Expr }
          deriving (Eq)

instance Show Expr where
  show (Var v) = v
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Lambda v e) = "(\\" ++ v ++ "." ++ show e ++ ")"

instance IsString Expr where
  fromString s = Var s

-- infixl 2 :@
-- pattern (:@) :: Expr -> Expr -> Expr
-- pattern e1 :@ e2 = App e1 e2

infixl 2 @@
(@@) :: Expr -> Expr -> Expr
(@@) = App

lambda :: String -> String
lambda = id

infixr 1 <.>
(<.>) :: String -> Expr -> Expr
x <.> e = Lambda x e


normalize :: Expr -> Expr
normalize = normalizeWith M.empty

normalizeWith :: M.Map String Expr -> Expr -> Expr
normalizeWith env e = e


-- some useful definitions
omega = lambda "x" <.> "x" @@ "x"

cons = lambda "x" <.> lambda "y" <.> lambda "f" <.> "f" @@ "x" @@ "y"
car = lambda "t" <.> "t" @@ (lambda "x" <.> lambda "_" <.> "x")
cdr = lambda "t" <.> "t" @@ (lambda "_" <.> lambda "y" <.> "y")

zero = lambda "f" <.> lambda "x" <.> "x"
succ = lambda "n" <.> lambda "f" <.> lambda "x" <.> "f" @@ ("n" @@ "f" @@ "x")


prelude = M.fromList [("omega", omega)
                    , ("cons", cons)
                    , ("car", car)
                    , ("cdr", cdr)]
