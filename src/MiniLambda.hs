module MiniLambda
    ( Expr(..),
      Name,
      Env,
      normalizeFull,
      normalizeFull',
      emptyEnv,
      normalizeN,
      normalizeStep,
      substitute,
      freeIn,
      freeVars,
      lambda,
      (<.>),
      (@@)
    ) where


import Prelude hiding (lookup)
import Data.String
import Data.Map (Map, empty, lookup, fromList, delete)
import Data.Set (Set, member, singleton, union, (\\))
import Control.Monad.State


type Name = String

data Expr = Var { name :: Name }
          | App Expr Expr
          | Lambda { arg :: Name, expr :: Expr }
          deriving (Eq)

type Env = Map Name Expr
type NameSet = Set Name


data EvalState = EvalState { environment :: Env, freeVars :: NameSet }

getEnv :: State EvalState Env
getEnv = gets environment

putEnv :: Env -> State EvalState ()
putEnv newEnv = do
       fv <- getFV
       put $ EvalState newEnv fv

modifyEnv :: (Env -> Env) -> State EvalState ()
modifyEnv f = do
          env <- getEnv
          putEnv (f env)

getFV :: State EvalState NameSet
getFV = gets freeVars

putFV :: NameSet -> State EvalState ()
putFV newFV = do
      env <- getEnv
      put $ EvalState env newFV

modifyFV :: (NameSet -> NameSet) -> State EvalState ()
modifyFV f = do
         fv <- getFV
         putFV (f fv)

genSym :: Name -> State EvalState Name
genSym n = do
       fv <- getFV
       if n `member` fv
            then genSym (n ++ "\'")
            else return n          



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



normalizeFull :: Env -> Expr -> Expr
normalizeFull env e = evalState (go e) init
           where init = EvalState env (freeVarsOf e)
                 go e = do
                   e' <- normalizeStep e
                   if e' == e
                      then return e
                      else go e'

normalizeFull' = normalizeFull emptyEnv

emptyEnv :: Env
emptyEnv = empty

normalizeN :: Env -> Int -> Expr -> Expr
normalizeN env n e = evalState (go e n) init
           where init = EvalState env (freeVarsOf e)
                 go e n = do
                   if n == 0
                      then return e
                      else do
                        e' <- normalizeStep e
                        go e' (n-1)
      

normalizeStep :: Expr -> State EvalState Expr
normalizeStep v@(Var x) = do
              env <- getEnv
              return $ case  lookup x env of
                             Nothing -> v
                             Just e -> e
normalizeStep (Lambda v e) = do
              e' <- do
                 modifyEnv (delete v)
                 normalizeStep e
              return $ Lambda v e'
normalizeStep (App v@(Var x) e2) = do
              env <- getEnv
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
substitute :: Name -> Expr -> Expr -> State EvalState Expr
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
    z <- genSym y
    t' <- substitute y (Var z) t
    substitute x s (Lambda z t')


freeIn :: Name -> Expr -> Bool
x `freeIn` e = x `member` (freeVarsOf e)
-- x `freeIn` (Var y) = x == y
-- x `freeIn` (Lambda y t) = x /= y && x `freeIn` t
-- x `freeIn` (App t1 t2) = x `freeIn` t1 || x `freeIn` t2

freeVarsOf :: Expr -> NameSet
freeVarsOf (Var x) = singleton x
freeVarsOf (Lambda x t) = freeVarsOf t \\ singleton x
freeVarsOf (App t1 t2) = (freeVarsOf t1) `union` (freeVarsOf t2)

-- ((\f. (\x. (f (f x)))) (\f. (\x. (f (f x)))))
