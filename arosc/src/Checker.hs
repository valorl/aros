module Checker where

import Syntax
import Types

import Data.Map(Map)
import qualified Data.Map as M
import Control.Monad

import Text.Show.Pretty (ppShow)


type Name = String
type Environment = Map Name Type

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


checkExp :: Monad m => Environment -> Exp -> Type -> m Bool
checkExp env exp typ =
  case exp of

    IntegerExp _ -> return $ TInteger == typ

    BooleanExp _ -> return $ TBoolean == typ

    ParenExp exp -> checkExp env exp typ

    VariableExp name ->
      case M.lookup name env of
        Nothing -> error ("Variable '" <> name <> "' not in scope.")
        Just t -> return $ t == typ

    VectorExp e1 e2 -> checkVector env e1 e2

    ListExp exps -> do
      let TList t = typ
      collOk <- checkColl env exps t
      if collOk
      then return True
      else error ("")

    SetExp exps -> do
      let TSet t = typ
      collOk <- checkColl env exps t
      if collOk
      then return True
      else error ("Found heterogenous set expression.")

    UnaryExp op exp -> do
      let errorMsg = "Unary operation '" <> (show op) <> "' applied to '"
                     <> (ppShow exp) <> "' was not " <> (show typ) <> "."
      ok <- case typ of
        TInteger -> checkVecUnaryOp env op exp
        TBoolean -> checkBoolUnaryOp env op exp
        TList _ -> checkListUnaryOp env op exp typ
        otherwise -> return False
      if ok
      then return True
      else error errorMsg

    BinaryExp l op r -> do
      let errorMsg = "Binary operation '" <> (show op) <> "' applied to '"
                      <> (ppShow l) <> "' and '" <> (ppShow r) <>  "' was not " <> (show typ) <> "."
      ok <- case typ of
        TInteger -> checkIntBinaryOp env op l r
        TVector -> do
          isVec <- checkVecBinaryOp env op l r
          isIntVec <- checkIntVecBinaryOp env op l r
          return (isVec || isIntVec)
        TBoolean -> do
          isBool <- checkBoolBinaryOp env op l r
          isCompare <- checkCompareBinaryOp env op l r
          return (isBool || isCompare)
        TList _ -> do
          isList <- checkListBinaryOp env op l r typ
          isCons <- checkConsBinaryOp env op l r typ
          return (isList || isCons)
        TSet _ -> do
          isSet <- checkSetBinaryOp env op l r typ
          isSetVec <- checkSetVecBinaryOp env op l r
          return (isSet || isSetVec)
        otherwise -> return False
      if ok
      then return True
      else error errorMsg

    IfExp cond b1 b2 -> do
      let errorMsg = "If expression type error:\n"
                   <> "Condition: " <> (show cond)
                   <> "\nTrue Block: " <> (show b1)
                   <> "\nFalseBlock: " <> (show b2)
      condOk <- checkExp env cond TBoolean
      b1ok <- checkBlock env b1 typ
      b2ok <- checkBlock env b2 typ
      let ok = and [condOk, b1ok, b2ok]
      if ok
      then return True
      else error errorMsg

    CondExp condBlocks otherwiseBlock -> do
      let (exps, blocks) = unzip condBlocks
      let errorMsg = "Cond expression type error:\n"
                   <> "Conditions: " <> (show exps)
                   <> "\nBlocks: " <> (show blocks)
                   <> "\nOtherwise block: " <> (show otherwiseBlock)
      expsOk <- mapM (\e -> checkExp env e TBoolean) exps
      blocksOk <- mapM (\b -> checkBlock env b typ) (otherwiseBlock : blocks)
      let ok = (and expsOk) && (and blocksOk)
      if ok
      then return True
      else error errorMsg






     


-- VECTOR
checkVector :: Monad m => Environment -> Exp -> Exp -> m Bool
checkVector env e1 e2 = do
  t1ok <- checkExp env e1 TInteger
  t2ok <- checkExp env e2 TInteger
  if t1ok && t2ok
  then return True
  else error ("Invalid vector expression. ")

-- LIST & SET
checkColl :: Monad m => Environment -> [Exp] -> Type -> m Bool
checkColl env exps typ = do
  oks <- mapM go exps
  let correct = and oks
  return $ (null exps) || correct
    where go exp = checkExp env exp typ

-- UNARY OPS
checkVecUnaryOp :: Monad m => Environment -> UnaryOp -> Exp -> m Bool
checkVecUnaryOp env op exp = do
  let opOk = op `elem` [Vecx, Vecy]
  expOk <- checkExp env exp TVector
  return $ opOk && expOk

checkBoolUnaryOp :: Monad m => Environment -> UnaryOp -> Exp -> m Bool
checkBoolUnaryOp env op exp = do
  let opOk = op `elem` [Not]
  expOk <- checkExp env exp TBoolean
  return $ opOk && expOk

checkListUnaryOp :: Monad m => Environment -> UnaryOp -> Exp -> Type -> m Bool
checkListUnaryOp env op exp typ = do
  let opOk = op `elem` [Head, Tail]
  expOk <- checkExp env exp typ
  return $ opOk && expOk

-- BINARY OPS
checkIntBinaryOp :: Monad m => Environment -> BinaryOp -> Exp -> Exp -> m Bool
checkIntBinaryOp env op e1 e2 = do
  let opOk = op `elem` [Plus, Minus, Times, Div]
  e1ok <- checkExp env e1 TInteger
  e2ok <- checkExp env e2 TInteger
  return $ and [opOk, e1ok, e2ok]

checkVecBinaryOp :: Monad m => Environment -> BinaryOp -> Exp -> Exp -> m Bool
checkVecBinaryOp env op e1 e2 = do
  let opOk = op `elem` [Plus, Minus, Times, Div]
  e1ok <- checkExp env e1 TVector
  e2ok <- checkExp env e2 TVector
  return $ and [opOk, e1ok, e2ok]

checkIntVecBinaryOp :: Monad m => Environment -> BinaryOp -> Exp -> Exp -> m Bool
checkIntVecBinaryOp env op e1 e2 = do
  let opOk = op `elem` [Times]
  e1ok <- checkExp env e1 TInteger
  e2ok <- checkExp env e2 TVector
  return $ and [opOk, e1ok, e2ok]

checkBoolBinaryOp :: Monad m => Environment -> BinaryOp -> Exp -> Exp -> m Bool
checkBoolBinaryOp env op e1 e2 = do
  let opOk = op `elem` [And, Or]
  e1ok <- checkExp env e1 TBoolean
  e2ok <- checkExp env e2 TBoolean
  return $ and [opOk, e1ok, e2ok]

checkCompareBinaryOp :: Monad m => Environment -> BinaryOp -> Exp -> Exp -> m Bool
checkCompareBinaryOp env op e1 e2 = do
  let opOk = op `elem` [Gt, Lt, Gte, Lte, Equal, NotEqual]
  e1ok <- checkExp env e1 TInteger
  e2ok <- checkExp env e2 TInteger
  return $ and [opOk, e1ok, e2ok]

checkListBinaryOp :: Monad m => Environment -> BinaryOp -> Exp -> Exp -> Type -> m Bool
checkListBinaryOp env op e1 e2 typ = do
  let opOk = op `elem` [Append]
  e1ok <- checkExp env e1 typ
  e2ok <- checkExp env e2 typ
  return $ and [opOk, e1ok, e2ok]

checkConsBinaryOp :: Monad m => Environment -> BinaryOp -> Exp -> Exp -> Type -> m Bool
checkConsBinaryOp env op e1 e2 typ = do
  let opOk = op `elem` [Cons]
  let (TList t) = typ
  e1ok <- checkExp env e1 t
  e2ok <- checkExp env e2 typ
  return $ and [opOk, e1ok, e2ok]

checkSetBinaryOp :: Monad m => Environment -> BinaryOp -> Exp -> Exp -> Type -> m Bool
checkSetBinaryOp env op e1 e2 typ = do
  let opOk = op `elem` [Union, Intersection]
  e1ok <- checkExp env e1 typ
  e2ok <- checkExp env e2 typ
  return $ and [opOk, e1ok, e2ok]

checkSetVecBinaryOp :: Monad m => Environment -> BinaryOp -> Exp -> Exp -> m Bool
checkSetVecBinaryOp env op e1 e2 = do
  let opOk = op `elem` [Shift, Crop]
  e1ok <- checkExp env e1 $ TSet TVector
  e2ok <- checkExp env e2 TVector
  return $ and [opOk, e1ok, e2ok]

-- BLOCK
checkBlock :: Monad m => Environment -> Block -> Type -> m Bool
checkBlock env (Block decs body) typ = do
  let varTypes = map (\(Decl dt name _) -> (name, (convertDeclType dt))) decs
  decsOk <- checkDeclarations env decs
  let localEnv = M.union (M.fromList(varTypes)) env
  checkExp localEnv body typ

checkDeclaration :: Monad m => Environment -> Declaration -> m Bool
checkDeclaration env (Decl dt name exp) = do
  checkExp env' exp typ
    where typ = convertDeclType dt
          env' = case typ of
                TFunction _ _ -> M.insert name typ env
                otherwise -> env

checkDeclarations :: Monad m => Environment -> [Declaration] -> m Bool
checkDeclarations env decs
  | Just _ <- results = return True
  | Nothing <- results = return False
  where
    results = foldl folder (Just env) decs
    folder Nothing _ = Nothing
    folder (Just env') dec@(Decl dt name _) = do
      let t = convertDeclType dt
      decOk <- checkDeclaration env' dec
      if decOk
      then Just (M.insert name t env')
      else Nothing

checkProgram :: Monad m => Environment -> Program -> m Bool
checkProgram env (Program decs grid rpath) = do
  let errorMsg = "Program (root) type error:"
                 <> "\nDeclarations: " <> (show decs)
                 <> "\nGrid: " <> (show grid)
                 <> "\nRobot Path: " <> (show rpath)
  let varTypes = map (\(Decl dt name _) -> (name, (convertDeclType dt))) decs
  decsOk <- checkDeclarations env decs
  let localEnv = M.union (M.fromList(varTypes)) env
  gridOk <- checkGrid localEnv grid
  rpathOk <- checkRobotPath localEnv rpath
  let ok = decsOk && gridOk && rpathOk
  if ok
  then return True
  else error errorMsg


checkGrid :: Monad m => Environment -> GridDef -> m Bool
checkGrid env (GridDef bounds points) = do
  boundsOk <- checkExp env bounds TVector
  pointsOk <- checkExp env points (TSet TVector)
  let errorMsg = "Grid definition type error:"
                 <> "\nBounds: " <> (show bounds)
                 <> "\nPoints: " <> (show points)
  if (boundsOk && pointsOk)
  then return True
  else error errorMsg

checkRobotPath :: Monad m => Environment -> RobotPath -> m Bool
checkRobotPath env (RobotPath path) = do
  pathOk <- checkExp env path (TList TVector)
  let errorMsg = "Robot path definition type error:"
                 <> "\nPath: " <> (show path)
  if pathOk
  then return True
  else error errorMsg



-- HELPERS
convertDeclType :: DeclType -> Type
convertDeclType dt = case dt of
  TypeInt -> TInteger
  TypeBool -> TBoolean
  TypeVec -> TVector
  TypeList t -> TList (convertDeclType t)
  TypeSet t -> TSet (convertDeclType t)
  TypeLambda params body ->
    TFunction (map convertDeclType params) (convertDeclType body)
