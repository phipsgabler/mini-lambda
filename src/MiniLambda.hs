{-# LANGUAGE OverloadedStrings #-}

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

infixl 2 @@
(@@) :: Expr -> Expr -> Expr
(@@) = App

lambda :: String -> String
lambda = id

infixr 1 <.>
(<.>) :: String -> Expr -> Expr
x <.> e = Lambda x e


normalize :: Expr -> Expr
normalize e = normalize' M.empty e
  where normalize' :: M.Map String Expr -> Expr -> Expr
        normalize' env x@(Var v) = case  M.lookup v env of
                                     Nothing -> x
                                     Just x' -> x'
        normalize' env (Lambda v e) = Lambda v (normalize' (M.delete v env) e)
        normalize' env (App v@(Var _) e2) = App (normalize' env v) (normalize' env e2)
        normalize' env (App (Lambda x e1) e2) = let e2' = normalize' env e2 
                                                in normalize' (M.insert x e2' env) e1
        normalize' env (App e1@(App _ _) e2) = case normalize' env e1 of
                                                 l@(Lambda _ _) -> normalize' env $ App l e2
                                                 n -> App n $ normalize' env e2


cons = lambda "x" <.> lambda "y" <.> lambda "f" <.> "f" @@ "x" @@ "y"
car = lambda "t" <.> "t" @@ (lambda "x" <.> lambda "_" <.> "x")
cdr = lambda "t" <.> "t" @@ (lambda "_" <.> lambda "y" <.> "y")
