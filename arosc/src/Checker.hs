{-# LANGUAGE BangPatterns #-}


module Checker where

import Syntax
import Types

import Data.List
import Data.Maybe
import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Text.Show.Pretty (ppShow, pPrint)


type Name = String
type Environment = Map Name Type

data LogMsg = Error String | Info String
  deriving Show


ifexp = IfExp (BooleanExp False)
  (Block
     [ Decl TypeInt "x" (IntegerExp 5)
     , Decl
         TypeInt "y" (BinaryExp (VariableExp "x") Plus (IntegerExp 5))
     ]
     (BinaryExp (VariableExp "x") Times (VariableExp "y")))
  (Block
     [ Decl TypeVec "myVec" (VectorExp (IntegerExp 5) (IntegerExp 6))
     , Decl
         TypeVec
         "myVec2"
         (BinaryExp (IntegerExp 10) Times (VariableExp "myVec"))
     ]
     (UnaryExp Vecy (VariableExp "myVec2")))

-- typeMismatchMsg :: String -> [(Type,Type)] -> String
-- typeMismatchMsg msg tuples = msg <> "\n" <> msgs
--   where mismatches = filter (\(x,y) -> x /= y) tuples
--         toMsg (expected, got) =
--           "Expected '" ++ (show expected) ++ "', but got '" ++ (show got) ++ "'."
--         msgs = concat $ intersperse "\n" (map toMsg mismatches)

-- typesMatch :: [(Type, Type)] -> Bool
-- typesMatch tuples = and $ map (\(x,y) -> x == y) tuples

logErr :: String -> Writer [LogMsg] ()
logErr str = tell [Error str]

type MWType = MaybeT (Writer [LogMsg]) Type
type MWBool = MaybeT (Writer [LogMsg]) Bool
type WBool = Writer [LogMsg] Bool

runChecker :: Program -> (Bool, [LogMsg])
runChecker prg =
  case result of
    Just r ->  if (anyError log)
               then (False, log)
               else (r, log)
    Nothing -> (False, log)
  where (result, log) = runWriter $ runMaybeT $ checkProgram mempty prg
        errors = [ err | err@(Error _) <- log]
        anyError log = (length errors) > 0






checkExp :: Environment -> Exp -> MWType
checkExp env exp = case exp of
    IntegerExp _ -> return TInteger

    BooleanExp _ -> return TBoolean

    ParenExp exp -> checkExp env exp

    VariableExp name ->
      case M.lookup name env of
        Nothing -> do
          lift $ logErr $ "Variable error: Variable '" <> name <> "' not in scope."
          mzero
        Just t -> return t

    VectorExp e1 e2 -> do
      t1 <- checkExp env e1
      t2 <- checkExp env e2
      let intsOk = (t1 == TInteger) && (t2 == TInteger)
      if (not intsOk)
      then do
        lift $ logErr $
          "Vector error: Expected <TInteger, TInteger> but got <" <> (show t1) <> ", " <> (show t2) <> ">"
        mzero
      else
        return TVector

    ListExp exps -> do
      if (null exps)
      then return $ TList TAny
      else do
        types@(t:ts) <- mapM (checkExp env) exps
        let homo = and $ map (== t) ts
        if homo
        then return $ TList t
        else do
          lift $ logErr $ "Found heterogenous list with types: " <> (show types)
          mzero

    SetExp exps -> do
      if (null exps)
      then return $ TSet TAny
      else do
        types@(t:ts) <- mapM (checkExp env) exps
        let homo = and $ map (== t) ts
        if homo
        then return $ TSet t
        else do
          lift $ logErr $ "Found heterogenous set with types: " <> (show types)
          mzero

    UnaryExp op exp -> do
      t <- checkExp env exp
      let checkers = [checkVecUnaryOp,
                      checkBoolUnaryOp,
                      checkListUnaryHeadOp, checkListUnaryTailOp]
      let results = map (\f -> f op t) checkers
      let successes = catMaybes results
      handleUnaryResults successes op t

    BinaryExp e1 op e2 -> do
      t1 <- checkExp env e1
      t2 <- checkExp env e2
      let checkers = [checkIntBinaryOp, checkVecBinaryOp, checkIntVecBinaryOp,
                      checkBoolBinaryOp, checkBoolCompareBinaryOp,
                      checkListBinaryConsOp, checkListBinaryOp,
                      checkSetVecBinaryOp, checkSetBinaryOp]
      let results = map (\f -> f t1 op t2) checkers
      let successes = catMaybes results
      handleBinaryResults successes op t1 t2

    IfExp cond b1 b2 -> do
      tCond <- checkExp env cond
      let condOk = tCond == TBoolean
      when (not condOk) $
        lift $ logErr $ "Invalid if expression: condition was " <> (show tCond)
      t1 <- checkBlock env b1
      t2 <- checkBlock env b2
      let blocksOk = t1 == t2
      when (not blocksOk) $
        lift $ logErr $ "Invalid if expression: inconsistent return types " <> (show t1) <> " and " <> (show t2)
      if condOk && blocksOk
      then return t1
      else mzero

    CondExp condBlocks otherwiseBlock -> do
      let (conds, blocks) = unzip condBlocks
      condTypes <- mapM (checkExp env) conds
      let condsOk = and $ map (== TBoolean) condTypes
      when (not condsOk) $
        lift $ logErr $ "Invalid condition(s) in cond expression: Condition types were " <> (show condTypes)
      blockTypes <- mapM (checkBlock env) (otherwiseBlock : blocks)
      let blocksOk = and $ map (== (head blockTypes)) blockTypes
      when (not blocksOk) $
        lift $ logErr $ "Invalid block(s) in cond expression: Blocks types were " <> (show condTypes)
      if condsOk && blocksOk
      then return (head blockTypes)
      else mzero

    LambdaExp args dtOut block -> do
      let tOut = convertDeclType dtOut
      let varTypes = map (\(dt, name) -> (name, convertDeclType dt)) args
      let localEnv = M.union (M.fromList(varTypes)) env
      tBlock <- checkBlock localEnv block
      let blockOk = tBlock == tOut
      when (not blockOk) $
        lift $ logErr $
          "Invalid lambda expression: Block declared as " <> (show tOut) <> " but was " <> (show tBlock)
      if blockOk
      then return $ TFunction ((snd . unzip) varTypes) tOut
      else mzero

    ApplicationExp fn params -> do
      tFn <- checkExp env fn
      let isFunc = case tFn of
                     TFunction _ _ -> True
                     otherwise -> False
      when (not isFunc) $ lift $ logErr $
          "Invalid application expression: Expected a function but got " <> (show tFn)
      let (TFunction tFnParams tOut) = tFn
      tParams <- mapM (checkExp env) params
      let paramsOk = tParams == (take (length tParams) tFnParams)
      when (not paramsOk) $ lift $ logErr $
          "Invalid application expression: Function (" <> (show tFn)
          <> ") applied with invalid parameter list" <> (show tParams)
      let curried = (length tParams) < (length tFnParams)
      if isFunc && paramsOk
      then return $
        if (length tParams) < (length tFnParams)
        then TFunction (drop (length tParams) tFnParams) tOut
        else tOut
      else mzero



checkProgram :: Environment -> Program -> MWBool
checkProgram env (Program decs grid route) = do
  localEnv <- lift $ checkDeclarations env decs
  gridOk <- checkGrid localEnv grid
  routeOk <- checkRoute localEnv route
  return $ gridOk && routeOk

checkGrid :: Environment -> GridDef -> MWBool
checkGrid env (GridDef bounds points) = do
  tBounds <- checkExp env bounds
  tPoints <- checkExp env points
  let boundsOk = tBounds == TVector
  when (not boundsOk) $
    lift $ logErr $ "Invalid grid definition: Expected bounds parameter to be a vector but was " <> (show tBounds)
  let pointsOk = tPoints == TSet TVector
  when (not pointsOk) $
    lift $ logErr $ "Invalid grid definition: Expected points parameter to be a vector set but was " <> (show tPoints)
  return $ boundsOk && pointsOk


checkRoute :: Environment -> RobotRoute -> MWBool
checkRoute env (RobotRoute start end) = do
  tStart <- checkExp env start
  tEnd <- checkExp env end
  let startOk = tStart == TVector
  when (not startOk) $
    lift $ logErr $ "Invalid grid definition: Expected start parameter to be a vector but was " <> (show tStart)
  let endOk = tEnd == TVector
  when (not endOk) $
    lift $ logErr $ "Invalid grid definition: Expected end parameter to be a vector but was " <> (show tEnd)
  return $ startOk && endOk




--UNARY OPS
checkVecUnaryOp :: UnaryOp -> Type -> Maybe Type
checkVecUnaryOp op inputType = do
  let opOk = op `elem` [Vecx, Vecy]
  let typOk = inputType == TVector
  if opOk && typOk
  then return TInteger
  else mzero

checkBoolUnaryOp :: UnaryOp -> Type -> Maybe Type
checkBoolUnaryOp op inputType = do
  let opOk = op `elem` [Not]
  let typOk = inputType == TBoolean
  if opOk && typOk
  then return TBoolean
  else mzero

checkListUnaryHeadOp :: UnaryOp -> Type -> Maybe Type
checkListUnaryHeadOp op inputType = do
  let opOk = op == Head
  let typ = case inputType of
              TList t -> Just t
              otherwise -> Nothing
  if opOk then typ else mzero

checkListUnaryTailOp :: UnaryOp -> Type -> Maybe Type
checkListUnaryTailOp op inputType = do
  let opOk = op == Tail
  let mbType = case inputType of
              lst@(TList t) -> Just lst
              otherwise -> Nothing
  if opOk then mbType else mzero

checkIntBinaryOp :: Type -> BinaryOp -> Type -> Maybe Type
checkIntBinaryOp t1 op t2 = do
  let opOk = op `elem` [Plus, Minus, Div, Times]
  let typesOk = (t1 == TInteger) && (t2 == TInteger)
  if opOk && typesOk
  then return TInteger
  else mzero

checkVecBinaryOp :: Type -> BinaryOp -> Type -> Maybe Type
checkVecBinaryOp t1 op t2 = do
  let opOk = op `elem` [Plus, Minus, Div, Times]
  let typesOk = (t1 == TVector) && (t2 == TVector)
  if opOk && typesOk
  then return TVector
  else mzero

checkIntVecBinaryOp :: Type -> BinaryOp -> Type -> Maybe Type
checkIntVecBinaryOp t1 op t2 = do
  let opOk = op `elem` [Times]
  let typesOk = (t1 == TInteger) && (t2 == TVector)
  if opOk && typesOk
  then return TVector
  else mzero

checkBoolBinaryOp :: Type -> BinaryOp -> Type -> Maybe Type
checkBoolBinaryOp t1 op t2 = do
  let opOk = op `elem` [And, Or]
  let typesOk = (t1 == TBoolean) && (t2 == TBoolean)
  if opOk && typesOk
  then return TBoolean
  else mzero

checkBoolCompareBinaryOp :: Type -> BinaryOp -> Type -> Maybe Type
checkBoolCompareBinaryOp t1 op t2 = do
  let opOk = op `elem` [Lt, Gt, Lte, Gte, Equal, NotEqual]
  let typesOk = (t1 == TInteger) && (t2 == TInteger)
  if opOk && typesOk
  then return TBoolean
  else mzero

checkListBinaryConsOp :: Type -> BinaryOp -> Type -> Maybe Type
checkListBinaryConsOp t1 op t2 = do
  let opOk = op == Cons
  let isList = case t2 of
                  TList _ -> True
                  otherwise -> False
  let (TList t) = t2
  if opOk && isList && (t1 == t)
  then return $ TList t1
  else mzero

checkListBinaryOp :: Type -> BinaryOp -> Type -> Maybe Type
checkListBinaryOp t1 op t2 = do
  let opOk = op `elem` [Append]
  let isList x = case x of
                  TList _ -> True
                  otherwise -> False
  let (TList inner1) = t1
  let (TList inner2) = t2
  if opOk && (isList t1) && (isList t2) && (inner1 == inner2)
  then return $ TList (selectNotAny inner1 inner2)
  else mzero

checkSetVecBinaryOp :: Type -> BinaryOp -> Type -> Maybe Type
checkSetVecBinaryOp t1 op t2 = do
  let opOk = op `elem` [Shift, Crop]
  let typesOk = (t1 == TSet TVector) && (t2 == TVector)
  if opOk && typesOk
  then return t1
  else mzero

checkSetBinaryOp :: Type -> BinaryOp -> Type -> Maybe Type
checkSetBinaryOp t1 op t2 = do
  let opOk = op `elem` [Union, Intersection]
  let isSet x = case x of
                  TSet _ -> True
                  otherwise -> False
  let (TSet inner1) = t1
  let (TSet inner2) = t2
  if opOk && (isSet t1) && (isSet t2) && (inner1 == inner2)
  then return $ TSet (selectNotAny inner1 inner2)
  else mzero


-- BLOCK
checkBlock :: Environment -> Block -> MWType
checkBlock env (Block decs exp) = do
  localEnv <- lift $ checkDeclarations env decs
  checkExp localEnv exp

checkDeclaration :: Environment -> Declaration -> Writer [LogMsg] ()
checkDeclaration env (Decl dt name exp) = do
  let expType = convertDeclType dt
  let env' = case expType of
               TFunction _ _ -> M.insert name expType env
               otherwise -> env
  realType <- runMaybeT $ checkExp env' exp
  case realType of
    (Just t) -> do
      let typeOk = expType == t
      when (not typeOk) $
        logErr $ "Invalid declaration: Expected " <> (show expType) <> " but got " <> (show t)
    Nothing -> return ()

checkDeclarations :: Environment -> [Declaration] -> Writer [LogMsg] Environment
checkDeclarations env decs = foldM folder env decs
  where
    folder :: Environment -> Declaration -> Writer [LogMsg] Environment
    folder env' dec@(Decl dt name _) = do
      let t = convertDeclType dt
      checkDeclaration env' dec
      return (M.insert name t env')


-- -- TODO
-- -- checkProgram :: Monad m => Environment -> Program -> m Bool
-- -- checkProgram env (Program decs grid rpath) = do
-- --   let errorMsg = "Program (root) type error:"
-- --                  <> "\nDeclarations: " <> (show decs)
-- --                  <> "\nGrid: " <> (show grid)
-- --                  <> "\nRobot Path: " <> (show rpath)
-- --   let varTypes = map (\(Decl dt name _) -> (name, (convertDeclType dt))) decs
-- --   decsOk <- checkDeclarations env decs
-- --   let localEnv = M.union (M.fromList(varTypes)) env
-- --   gridOk <- checkGrid localEnv grid
-- --   rpathOk <- checkRobotRoute localEnv rpath
-- --   let ok = decsOk && gridOk && rpathOk
-- --   if ok
-- --   then return True
-- --   else error errorMsg


-- checkGrid :: Monad m => Environment -> GridDef -> m Bool
-- checkGrid env (GridDef bounds points) = do
--   boundsOk <- checkExp env bounds TVector
--   pointsOk <- checkExp env points (TSet TVector)
--   let errorMsg = "Grid definition type error:"
--                  <> "\nBounds: " <> (show bounds)
--                  <> "\nPoints: " <> (show points)
--   if (boundsOk && pointsOk)
--   then return True
--   else error errorMsg

-- -- TODO
-- checkRobotRoute :: Monad m => Environment -> RobotRoute -> m Bool
-- checkRobotRoute env (RobotRoute start end) = do
--   pathOk <- checkExp env path (TList TVector)
--   let errorMsg = "Robot path definition type error:"
--                  <> "\nPath: " <> (show path)
--   if pathOk
--   then return True
--   else error errorMsg



-- HELPERS
handleUnaryResults :: [Type] -> UnaryOp -> Type -> MWType
handleUnaryResults xs op t
  | ((length xs) == 1) = return (head xs)
  | ((length xs) > 1) = do
      lift $ logErr $ "Ambiguous unary expression (Operation " <> (show op) <> "). Possible types: " <> (show xs)
      mzero
  | (null xs) = do
      lift $ logErr $ "Invalid unary expression (Operation " <> (show op) <> " applied to " <> (show t) <> ")."
      mzero

handleBinaryResults :: [Type] -> BinaryOp -> Type -> Type -> MWType
handleBinaryResults xs op t1 t2
  | ((length xs) == 1) = return (head xs)
  | ((length xs) > 1) = do
      lift $ logErr $ "Ambiguous binary expression (Operation " <> (show op) <> "). Possible types: " <> (show xs)
      mzero
  | (null xs) = do
      lift $ logErr $
        "Invalid binary expression (Operation " <> (show op) <>
        " applied to " <> (show t1) <> " and " <> (show t2) <> ")."
      mzero

convertDeclType :: DeclType -> Type
convertDeclType dt = case dt of
  TypeInt -> TInteger
  TypeBool -> TBoolean
  TypeVec -> TVector
  TypeList t -> TList (convertDeclType t)
  TypeSet t -> TSet (convertDeclType t)
  TypeLambda params body ->
    TFunction (map convertDeclType params) (convertDeclType body)
