{-# LANGUAGE FlexibleContexts #-}
-- |
module Inferer where

import Syntax
import Types

import Data.Map(Map)
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.State.Strict

-- infer :: Environment -> Exp -> [TypeVariable] -> Type
-- infer env exp stream =
--   case evalState (checkExp env exp) stream of
--     (typ, cons) ->
--       let s = evalState (unify (S.toList cons)) ()
--       in replace s l

checkExp :: MonadState ([TypeVariable]) m
  => Environment -> Exp -> m (Type, Constraints)
checkExp env exp =
  case exp of
    IntegerExp _ -> return (ConstType TInteger, mempty)
    BooleanExp _ -> return (ConstType TBoolean, mempty)

    ParenExp e -> checkExp env e

    VariableExp name ->
      case M.lookup name env of
        Nothing -> error ("Variable '" <> name <> "' not in scope.")
        Just typ -> return (typ, mempty)

    -- LambdaExp params block -> do
    --   pTypeVars <- replicateM (length params) genTypeVariable
    --   let pTypes = map VarType pTypeVars
    --   let localEnv = M.union env (M.fromList $ zip params pTypes)
    --   (rType, cons) <- checkBlock localEnv block
    --   return (FuncType pTypes rType, cons)

    -- VectorExp e1 e2 -> do
    --   (t1, cons1) <- checkExp env e1
    --   (t2, cons2) <- checkExp env e2
    --   tVar <- genTypeVariable
    --   let vecType = ConstType TVector
    --       intCons = S.fromList [(t1, ConstType TInteger),(t2, ConstType TInteger)]
    --       cons = intCons <> cons1 <> cons2
    --   return (vecType, cons)

    ListExp exps -> do
      results <- mapM (checkExp env) exps
      let (t:ts, conses) = unzip results
      let homogenCons = S.fromList $ zip (repeat t) ts
      let listType = ListType t
      let cons = homogenCons <> (S.unions conses)
      return (listType, cons)

















checkBlock :: MonadState ([TypeVariable]) m
  => Environment -> Block -> m (Type, Constraints)
checkBlock env block = error "checkBlock not implemented yet"









unify :: Monad m => [Constraint] -> m (Map TypeVariable Type)
unify [] = return mempty
unify ((a,b):rest)
  | a == b = unify rest
  | VarType v <- a = unifyVariable v rest a b
  | VarType v <- b = unifyVariable v rest b a
  | otherwise = error ("'" <> (show a) <> "' does not match '" <> show b <> "'.")


unifyVariable :: Monad m => TypeVariable -> [(Type, Type)] -> Type -> Type -> m (Map TypeVariable Type)
unifyVariable v cons a b =
  if occurs v b
  then error
       ("Occurs error:" <> (show a) <> " ~ " <> (show b) <> ".")
  else let subbed = M.singleton v b
       in do rest <- unify (substitute subbed cons)
             return (rest <> subbed)

occurs :: TypeVariable -> Type -> Bool
occurs x (VarType y)
  | x == y = True
  | otherwise = False
occurs _ (ConstType t) = False
occurs a b = error "Occurs check not implemented for these types"

substitute :: Map TypeVariable Type -> [(Type, Type)] -> [(Type, Type)]
substitute subs = map go
  where go (a,b) = (replace subs a, replace subs b)

replace :: Map TypeVariable Type -> Type -> Type
replace s' t' = M.foldrWithKey go t' s'
  where
    go s1 t (VarType s2)
      | s1 == s2 = t
      | otherwise = VarType s2
    go _ _ (ConstType t) = ConstType t
    go _ _ _ = error "replace not implemeted for these types"





genTypeVariable :: MonadState ([TypeVariable]) m => m TypeVariable
genTypeVariable = do
  vars <- get
  case vars of
    (v:vs) -> do
      put vs
      return v
    otherwise ->
      error "Out of type variables."
