{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}


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

runChecker :: Exp -> IO ()
runChecker exp = do
  let (result, log) = runWriter $ runMaybeT $ checkExp mempty exp
  pPrint result
  print log



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

    -- TODO
    -- ApplicationExp name params -> do
    --   let mbType = M.lookup name env
    --   let (Just fnType) = mbType
    --   let isFnType = case fnType of
    --                    TFunction _ _ -> True
    --                    otherwise -> False
    --   let (TFunction pTypes bType) = fnType
    --   let paramsMatch = (length pTypes) == (length params)

    --   let msgNotFound = "Function application error: Variable not found: '" <> (show name) <> "'"
    --   let msgNotFunction = "Expected variable '" <> (show name)
    --                        <> "' to be a function, but was '" <> (show fnType) <> "'."
    --   let msgParamsMismatch = "Expected function '" <> (show name)
    --                             <> "' to be applied to " <> (show $ length pTypes)
    --                             <> "' but was '" <> (show $ length params) <> " instead."
    --   let generalMsg = "Function application type error:"
    --                     <> "\nName:" <> (show name)
    --                     <> "\nParams: " <> (show params)
    --                     <> "\nFunction type: " <> (show fnType)
    --                     <> "\nExpected type: " <> (show typ)

    --   if isNothing mbType
    --   then error msgNotFound
    --   else if not isFnType
    --        then error msgNotFunction
    --        else if not paramsMatch
    --        then error msgParamsMismatch
    --        else do
    --          let outputOk = bType == typ
    --          let typedParams = zip params pTypes
    --          paramsOk <- mapM (\(p, t) -> checkExp env p t) typedParams
    --          if outputOk && (and paramsOk)
    --          then return True
    --          else error generalMsg

-- checkIntegerExp ::Environment -> Exp -> Writer [LogMsg] Bool
-- checkIntegerExp env exp = do
--   typ <- checkExp env exp
--   return $ typ == TInteger

-- checkColl :: Environment  -> [Exp] -> Type -> MWBool
-- checkColl env exps typ = do
--   oks <- mapM go exps
--   return $ and oks
--     where go exp = checkExp env exp

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
  then return t2
  else mzero

checkListBinaryOp :: Type -> BinaryOp -> Type -> Maybe Type
checkListBinaryOp t1 op t2 = do
  let opOk = op `elem` [Append]
  let isList x = case x of
                  TList _ -> True
                  otherwise -> False
  if opOk && (isList t1) && (isList t2) && (t1 == t2)
  then return t1
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
                  TList _ -> True
                  otherwise -> False
  if opOk && (isSet t1) && (isSet t2) && (t1 == t2)
  then return t1
  else mzero


-- BLOCK
checkBlock :: Environment -> Block -> MWType
checkBlock env (Block decs body) = do
  let varTypes = map (\(Decl dt name _) -> (name, (convertDeclType dt))) decs
  lift $ checkDeclarations env decs
  let localEnv = M.union (M.fromList(varTypes)) env
  checkExp localEnv body

checkDeclaration :: Environment -> Declaration -> WBool
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
      return typeOk
    Nothing -> return False

checkDeclarations :: Environment -> [Declaration] -> Writer [LogMsg] ()
checkDeclarations env decs = foldM_ folder ([],env) decs
  where
    folder :: ([Bool], Environment) -> Declaration -> Writer [LogMsg] ([Bool], Environment)
    folder (res, env') dec@(Decl dt name _) = do
      let t = convertDeclType dt
      decOk <- checkDeclaration env' dec
      let env'' = if decOk
                  then M.insert name t env'
                  else env'
      return (res ++ [decOk], env'')


-- -- BLOCK
-- checkBlock :: Monad m => Environment -> Block -> Type -> m Bool
-- checkBlock env (Block decs body) typ = do
--   let varTypes = map (\(Decl dt name _) -> (name, (convertDeclType dt))) decs
--   decsOk <- checkDeclarations env decs
--   let localEnv = M.union (M.fromList(varTypes)) env
--   checkExp localEnv body typ

-- checkDeclaration :: Monad m => Environment -> Declaration -> m Bool
-- checkDeclaration env (Decl dt name exp) = do
--   checkExp env' exp typ
--     where typ = convertDeclType dt
--           env' = case typ of
--                 TFunction _ _ -> M.insert name typ env
--                 otherwise -> env


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
