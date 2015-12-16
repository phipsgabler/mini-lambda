{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

module MiniLambda
    ( Expr(..),
      normalize,
      normalizeWith,
      substitute,
      freeIn,
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

type Env = M.Map String Expr

instance Show Expr where
  show (Var v) = v
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Lambda v e) = "(λ" ++ v ++ "." ++ show e ++ ")"

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

normalizeStep :: Env -> Expr -> Expr
normalizeStep env v@(Var x) = case  M.lookup x env of
                                Nothing -> v
                                Just e -> e
normalizeStep env (Lambda v e) = Lambda v (normalizeStep (M.delete v env) e)
normalizeStep env (App v@(Var x) e2) = case  M.lookup x env of
                                         Nothing -> App v (normalizeStep env e2)
                                         Just e -> App e e2
normalizeStep env (App (Lambda x e1) e2) = substitute x e2 e1
normalizeStep env (App e1@(App _ _) e2) = App (normalizeStep env e1) e2

normalizeWith :: Env -> Expr -> Expr
normalizeWith env e = if e == e'
                      then e
                      else normalizeWith env e'
              where e' = normalizeStep env e


-- normalizeWith :: Env -> Expr -> Expr
-- normalizeWith env x@(Var v) = case  M.lookup v env of
--   Nothing -> x
--   Just x' -> x'
-- normalizeWith env (Lambda v (App f (Var v'))) | v == v' = f            -- eta minimality
-- normalizeWith env (Lambda v e) = Lambda v (normalizeWith (M.delete v env) e)
-- normalizeWith env (App v@(Var _) e2) = case normalizeWith env v of
--   l@(Lambda _ _) -> normalizeWith env $ App l e2
--   n -> App n $ normalizeWith env e2
-- normalizeWith env (App (Lambda x e1) e2) = let e2' = normalizeWith env e2 
--                                            in normalizeWith (M.insert x e2' env) e1
-- normalizeWith env (App e1@(App _ _) e2) = case normalizeWith env e1 of
--   l@(Lambda _ _) -> normalizeWith env $ App l e2
--   n -> App n $ normalizeWith env e2

substitute :: String -> Expr -> Expr -> Expr
substitute x s (Var v) | x == v = s
                         | otherwise = Var v
substitute x s l@(Lambda y t) | y /= x && not (y `freeIn` s) = Lambda y (substitute x s t)
                              | otherwise = l
substitute x s a@(App t1 t2) = App (substitute x s t1) (substitute x s t2)

freeIn :: String -> Expr -> Bool
x `freeIn` (Var y) = x == y
x `freeIn` (Lambda y t) = x /= y && x `freeIn` t
x `freeIn` (App t1 t2) = x `freeIn` t1 || x `freeIn` t2


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
