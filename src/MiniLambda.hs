module MiniLambda
    ( Expr(..),
      Name,
      Env,
      evalFull,
      evalFullWith,
      evalN,
      evalNWith,
      evalStep,
      evalStepWith,
      substitute,
      freeIn,
      freeVarsOf,
      lambda,
      (<.>),
      (@@)
    ) 
    where


import Prelude hiding (lookup)
import Data.String
import Data.Map (Map, empty, lookup, fromList, delete)
import Data.Set (Set, member, singleton, union, (\\))
import Control.Monad.Reader (Reader, ask, local, runReader)


type Name = String

data Expr = Var { name :: Name }
          | App Expr Expr
          | Lambda { arg :: Name, expr :: Expr }
          deriving (Eq)

type Env = Map Name Expr
type NameSet = Set Name

environment :: Reader Env Env
environment = ask

instance Show Expr where
  show (Var v) = v
  show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Lambda v e) = "(λ" ++ v ++ "." ++ show e ++ ")"

instance IsString Expr where
  fromString s = Var s


infixl 2 @@
(@@) :: Expr -> Expr -> Expr
(@@) = App

lambda :: String -> Name
lambda = id

infixr 1 <.>
(<.>) :: Name -> Expr -> Expr
x <.> e = Lambda x e




evalFull :: Expr -> Expr
evalFull expr = evalFullWith empty expr

evalFullWith :: Env -> Expr -> Expr
evalFullWith env expr = runReader(normalizeFull expr) env

evalN :: Int -> Expr -> Expr
evalN n expr = evalNWith n empty expr

evalNWith :: Int -> Env -> Expr -> Expr
evalNWith n env expr = runReader (normalizeN n expr) env

evalStep :: Expr -> Expr
evalStep expr = evalStepWith empty expr

evalStepWith :: Env -> Expr -> Expr
evalStepWith env expr = runReader (normalizeStep expr) env


normalizeFull :: Expr -> Reader Env Expr
normalizeFull e = do
              e' <- normalizeStep e
              if e' == e
                 then return e
                 else normalizeFull e'

normalizeN :: Int -> Expr -> Reader Env Expr
normalizeN n e = do
           if n == 0
              then return e
              else do
                e' <- normalizeStep e
                normalizeN (n-1) e'
      

normalizeStep :: Expr -> Reader Env Expr
normalizeStep v@(Var x) = do
              env <- environment
              return $ case lookup x env of
                            Nothing -> v
                            Just e -> e
normalizeStep (Lambda v e) = do
              e' <- local (delete v) $ normalizeStep e
              return $ Lambda v e'
normalizeStep (App v@(Var x) e2) = do
              env <- environment
              case lookup x env of
                   Nothing -> do
                           e2' <- normalizeStep e2
                           return $ App v e2'
                   Just e -> return $ App e e2
normalizeStep (App (Lambda x e1) e2) = substitute x e2 e1
normalizeStep (App e1@(App _ _) e2) = do
              e1' <- normalizeStep e1
              return $ App e1' e2


-- See "The Implementation of Functional Programming Languages", p. 22
substitute :: Name -> Expr -> Expr -> Reader Env Expr
substitute x s (Var v) 
  | x == v = return s
  | otherwise = return $ Var v
substitute x s a@(App t1 t2) = do
           t1' <- substitute x s t1
           t2' <- substitute x s t2
           return $ App t1' t2'
substitute x s l@(Lambda y t) 
  | y == x = return l
  | y /= x && not (y `freeIn` s) = do
    l' <- substitute x s t
    return $ Lambda y l'
  | otherwise = do
    let z = genSym y (freeVarsOf t `union` freeVarsOf s)
    t' <- substitute y (Var z) t
    substitute x s (Lambda z t')


freeIn :: Name -> Expr -> Bool
x `freeIn` e = x `member` (freeVarsOf e)

freeVarsOf :: Expr -> NameSet
freeVarsOf (Var x) = singleton x
freeVarsOf (Lambda x t) = freeVarsOf t \\ singleton x
freeVarsOf (App t1 t2) = (freeVarsOf t1) `union` (freeVarsOf t2)

genSym :: Name -> NameSet -> Name
genSym n fv
       | n `member` fv = genSym (n ++ "\'") fv
       | otherwise =  n

