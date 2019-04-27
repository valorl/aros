module Checker where

import Syntax
import Types

import Data.Map(Map)
import qualified Data.Map as M

-- checkExp :: Environment -> Exp -> Type
-- checkExp env exp =
--   case exp of

--     IntegerExp _ -> TInteger

--     BooleanExp _ -> TBoolean

--     ParenExp exp -> checkExp env exp

--     VariableExp name ->
--       case M.lookup name env of
--         Nothing -> error ("Variable '" <> name <> "' not in scope.")
--         Just typ -> typ

--     VectorExp e1 e2 ->
--       let t1 = checkExp env e1
--           t2 = checkExp env e2
--           correct = and $ [t1 == TInteger, t2 == TInteger, t1 == t2]
--       in
--         if correct
--         then TVector
--         else error ("Invalid vector expression composed of "
--                     <> (show t1) <> "," <> (show t2) <> ".")

--     ListExp exps ->
--       let (t:ts) = map (checkExp env) exps
--           homogenous = and $ map (== t) ts
--       in
--         if homogenous
--         then TList t
--         else error ("Found heterogenous list expression.")

--     SetExp exps ->
--       let (t:ts) = map (checkExp env) exps
--           homogenous = and $ map (== t) ts
--       in
--         if homogenous
--         then TSet t
--         else error ("Found heterogenous set expression.")



--     UnaryExp op exp ->
--       let typ = checkExp env exp
--           boolOps = [Not]
--           listOps = [Head, Tail]
--           vecOps = [Vecx, Vecy]
--           opIn ops = op `elem` ops
--           errorMsg =
--             "Invalid unary operation (" <> (show op) <> ") applied to " <> (show typ) <> "."
--       in case typ of
--         TBoolean ->
--           if opIn boolOps
--           then TBoolean
--           else error errorMsg

--     BinaryExp l op r ->
--       let lType = checkExp env l
--           rType = checkExp env r
--           intOps = [Plus, Minus, Times, Div]
--           vecOps = [Plus, Minus, Times, Div]
--           intVecOps = [Times]
--           boolOps = [And, Or]
--           compOps = [Gt, Lt, Gte, Lte, Equal, NotEqual]
--           listOps = [Append]
--           anyListOps = [Cons]
--           setOps = [Union, Intersection]
--           setVecOps = [Shift, Crop]
--           opIn ops = op `elem` ops
--           errorMsg =
--             "Invalid binary operation (" <> (show op) <> ") applied to "
--             <> (show lType) <> " and " <> (show rType) <> "."
--       in case (lType, rType) of
--         (TInteger, TInteger) ->
--           if opIn intOps
--           then TInteger
--           else
--             if opIn compOps
--             then TBoolean
--             else error errorMsg
--         (TBoolean, TBoolean) ->
--           if opIn boolOps
--           then TBoolean
--           else error errorMsg
--         (TVector, TVector) ->
--           if opIn vecOps
--           then TVector
--           else error errorMsg
--         (TInteger, TVector) ->
--           if opIn intVecOps
--           then TVector
--           else error errorMsg
--         (TList t1, TList t2) ->
--           if not $ opIn listOps
--           then error errorMsg
--           else
--             if t1 /= t2
--             then error $ errorMsg <> "(Type mismatch)"
--             else TList t1
--         (_, TList t) ->
--           if not $ opIn anyListOps
--           then error errorMsg
--           else
--             if lType /= t
--             then error $ errorMsg <> "(Type mismatch)"
--             else TList t
--         (TSet t1, TSet t2) ->
--           if not $ opIn setOps
--           then error errorMsg
--           else
--             if t1 /= t2
--             then error $ errorMsg <> "(Type mismatch)"
--             else TSet t1
--         (TSet TVector, TVector) ->
--           if opIn setVecOps
--           then TSet TVector
--           else error errorMsg
--         otherwise ->
--           error errorMsg
