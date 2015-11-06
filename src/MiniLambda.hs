{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

module MiniLambda
    ( Expr(..),
      normalize,
      lambda,
      (<.>),
      (@@)
    ) where

import Data.String
import qualified Data.Map.Strict as M

data Expr = Var { name :: String }
          | App Expr Expr
          | Lambda { var:: String, expr :: Expr }
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
normalize e = reduce M.empty e
  where reduce :: M.Map String Expr -> Expr -> Expr
        reduce env x@(Var v) = case  M.lookup v env of
                                 Nothing -> x
                                 Just x' -> x'
        reduce env (Lambda v (App f (Var v'))) | v == v' = f            -- eta minimality
        reduce env (Lambda v e) = Lambda v (reduce (M.delete v env) e)
        reduce env (App v@(Var _) e2) = case (reduce env v) of
                                          l@(Lambda _ _) -> reduce env $ App l e2
                                          n -> App n $ reduce env e2
        reduce env (App (Lambda x e1) e2) = let e2' = reduce env e2 
                                            in reduce (M.insert x e2' env) e1
        reduce env (App e1@(App _ _) e2) = case reduce env e1 of
                                             l@(Lambda _ _) -> reduce env $ App l e2
                                             n -> App n $ reduce env e2


omega = (lambda "x" <.> "x" @@ "x")

cons = lambda "x" <.> lambda "y" <.> lambda "f" <.> "f" @@ "x" @@ "y"
car = lambda "t" <.> "t" @@ (lambda "x" <.> lambda "_" <.> "x")
cdr = lambda "t" <.> "t" @@ (lambda "_" <.> lambda "y" <.> "y")

zero = lambda "f" <.> lambda "x" <.> "x"
succ = lambda "n" <.> lambda "f" <.> lambda "x" <.> "f" @@ ("n" @@ "f" @@ "x")
