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
normalizeWith env x@(Var v) = case  M.lookup v env of
  Nothing -> x
  Just x' -> x'
normalizeWith env (Lambda v (App f (Var v'))) | v == v' = f            -- eta minimality
normalizeWith env (Lambda v e) = Lambda v (normalizeWith (M.delete v env) e)
normalizeWith env (App v@(Var _) e2) = case normalizeWith env v of
  l@(Lambda _ _) -> normalizeWith env $ App l e2
  n -> App n $ normalizeWith env e2
normalizeWith env (App (Lambda x e1) e2) = let e2' = normalizeWith env e2 
                                           in normalizeWith (M.insert x e2' env) e1
normalizeWith env (App e1@(App _ _) e2) = case normalizeWith env e1 of
  l@(Lambda _ _) -> normalizeWith env $ App l e2
  n -> App n $ normalizeWith env e2


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
