module Evaluator where
import Parser
import Syntax
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


data Value = TInt Int
           | TVec (Int, Int)
           | TBool Bool
           | TList [Value]
           | TSet (Set Value)
           | TLambda (Map String Value) [String] Block
           deriving (Show, Eq, Ord)


parsed :: Either String Program
parsed = Parser.parseAros "" "int myint = 5 + 3 * 2 ; vec thevec = <myint,2> ; {vec} place = {thevec} >> <3,3> ;  grid < 2 , 4 > , { <1,1> } routeRobot [ <1,1> ] "

evalTree :: Either String Program -> Either String String
evalTree (Right x) = evaluateProgram x Map.empty
evalTree (Left _) = Left "err"

-- Parses definitions into a map, then calls handleRobot
evaluateProgram :: Program -> Map String Value -> Either String String
evaluateProgram (Program ((Decl dtype ident expr):xs) grd wpts ) defs =
  case (computeDecl dtype defs expr) of
    Right computedDecl -> evaluateProgram (Program xs grd wpts) (Map.insert ident computedDecl defs)
    Left e -> Left e
evaluateProgram (Program [] grd wpts) defs = handleRobot (handleGrid grd defs) wpts defs


computeDecl :: DeclType -> Map String Value -> Exp -> Either String Value
computeDecl dtype defs expr = handleExp defs expr

-- TODO
handleGrid :: GridDef -> Map String Value -> Either String Value
handleGrid (GridDef e1 e2) defs = do
    playsize <- handleExp defs e1
    playmap <- handleExp defs e2
    return playmap

testVexp :: Exp
testVexp = VectorExp (IntegerExp 69) (IntegerExp 420)
testListExp :: Exp
testListExp = ListExp [(IntegerExp 1), (IntegerExp 2), (IntegerExp 3)]
testSetExp :: Exp
testSetExp = SetExp [(IntegerExp 1), (IntegerExp 2), (IntegerExp 3)]

handleExp :: Map String Value -> Exp -> Either String Value
handleExp defs (VariableExp ident) =
  case ( Map.lookup ident defs ) of
    (Just d) -> return d
    Nothing -> (Left "err")
handleExp defs (ParenExp expr) = handleExp defs expr
handleExp _ (IntegerExp i) = Right $ TInt i
handleExp _ (BooleanExp b) = Right $ TBool b
handleExp defs (VectorExp a b) = do
    ua <- handleExp defs a
    ub <- handleExp defs b
    case (ua, ub) of
      ((TInt ia),(TInt ib)) -> return (TVec (ia, ib))
      _ -> Left "err"

handleExp defs (ListExp expList) = do
  mapped <- mapM (handleExp defs) expList
  return $ TList mapped

handleExp defs (SetExp expSet) = do
  mapped <- mapM (handleExp defs) expSet
  return $ TSet (Set.fromList mapped)

handleExp defs (BinaryExp exp1 bop exp2) = do
  e1 <- handleExp defs exp1
  e2 <- handleExp defs exp2
  binaryOperationHandler bop e1 e2

handleExp defs (UnaryExp uop expr) = do
  e <- handleExp defs expr
  unaryExpressionHandler uop e

handleExp defs (LambdaExp strings block) = Right $ TLambda defs strings block

--TODO
handleExp defs (ApplicationExp expr (x:xs)) = do
  e <- handleExp defs expr
  return e

handleExp defs (IfExp expr block1  block2) =
  let (Right (TBool evaluated)) = handleExp defs expr in
    if evaluated
      then blockHandler defs block1
      else blockHandler defs block2

handleExp defs (CondExp ((expr,block):xs) otherwiseBlock) =
  let (Right (TBool evaluated)) = handleExp defs expr in
    if evaluated
      then blockHandler defs block
      else handleExp defs (CondExp xs otherwiseBlock)
handleExp defs (CondExp [] otherwiseBlock) = blockHandler defs otherwiseBlock

handleExp _ _ = Left "err"

blockHandler :: Map String Value -> Block -> Either String Value
blockHandler defs (Block ((Decl dtype ident expr):xs) finalExp) =
  case (computeDecl dtype defs expr) of
    Left _ -> Left "err"
    Right computedDecl -> blockHandler (Map.insert ident computedDecl defs) (Block xs finalExp)
blockHandler defs (Block [] finalExp) = handleExp defs finalExp


binaryOperationHandler :: BinaryOp -> Value -> Value -> Either String Value
binaryOperationHandler Plus  (TInt i) (TInt j) = Right $ TInt $ i+j
binaryOperationHandler Minus (TInt i) (TInt j) = Right $ TInt $ i-j
binaryOperationHandler Times (TInt i) (TInt j) = Right $ TInt $ i*j
binaryOperationHandler Div   (TInt i) (TInt j) = Right $ TInt $ div i j
binaryOperationHandler Cons i (TList xs)  = Right $ TList $ i:xs
binaryOperationHandler Append i (TList xs) = Right $ TList $ reverse $ i : ( reverse xs )
binaryOperationHandler Union (TSet s1) (TSet s2) = Right $ TSet $ Set.union s1 s2
binaryOperationHandler Intersection (TSet s1) (TSet s2) = Right $ TSet $ Set.intersection s1 s2
binaryOperationHandler Shift (TSet s) (TVec (a,b)) = Right $ TSet $ Set.map (\(TVec (x,y)) -> TVec (x+a, y+b)) s
binaryOperationHandler Crop (TSet s) (TVec (a,b)) =  Right $ TSet $ Set.filter (\(TVec (x,y)) -> x<=a && y<=b) s
binaryOperationHandler And (TBool b1) (TBool b2) = Right $ TBool $ b1 == b2
binaryOperationHandler Or (TBool b1) (TBool b2) = Right $ TBool $ b1 || b2
binaryOperationHandler Equal (TBool b1) (TBool b2) = Right $ TBool $ b1 == b2
binaryOperationHandler NotEqual (TBool b1) (TBool b2) = Right $ TBool $ b1 /= b2
binaryOperationHandler Equal (TInt i) (TInt j) = Right $ TBool $ i == j
binaryOperationHandler NotEqual (TInt i) (TInt j) = Right $ TBool $ i /= j
binaryOperationHandler Gt (TInt i) (TInt j) = Right $ TBool $ i > j
binaryOperationHandler Lt (TInt i) (TInt j) = Right $ TBool $ i < j
binaryOperationHandler Gte (TInt i) (TInt j) = Right $ TBool $ i >= j
binaryOperationHandler Lte (TInt i) (TInt j) = Right $ TBool $ i <= j
binaryOperationHandler Equal (TVec (i1,j1)) (TVec (i2,j2)) = Right $ TBool $ ( i1 == j1 ) && ( i2 ==  j2)
binaryOperationHandler NotEqual (TVec (i1,j1)) (TVec (i2,j2)) = Right $ TBool $  ( i1 /= j1 ) || ( i2 /=  j2)
binaryOperationHandler Gt (TVec (i1,j1)) (TVec (i2,j2)) = Right $ TBool $  ( i1 > j1 ) && ( i2 >  j2)
binaryOperationHandler Lt (TVec (i1,j1)) (TVec (i2,j2)) = Right $ TBool $  ( i1 < j1 ) && ( i2 <  j2)
binaryOperationHandler Gte (TVec (i1,j1)) (TVec (i2,j2)) = Right $ TBool $  ( i1 >= j1 ) && ( i2 >=  j2)
binaryOperationHandler Lte (TVec (i1,j1)) (TVec (i2,j2)) = Right $ TBool $  ( i1 <= j1 ) && ( i2 <=  j2)
binaryOperationHandler _ _ _ = Left "err"

unaryExpressionHandler :: UnaryOp -> Value -> Either String Value
unaryExpressionHandler Not (TBool e) = Right $ TBool $ not e
unaryExpressionHandler Head (TList (x:_)) = Right x
unaryExpressionHandler Tail (TList (_:xs)) = Right $ TList xs
unaryExpressionHandler Vecx (TVec (a,_)) = Right $ TInt a
unaryExpressionHandler Vecy (TVec (_,b)) = Right $ TInt b
unaryExpressionHandler _ _ = Left "err"


-- TODO
handleRobot :: Either String Value -> [Exp] -> Map String Value -> Either String String
handleRobot (Right (TSet s)) wpts defs = Right (show defs)
handleRobot _ _ _ = Left "err"
