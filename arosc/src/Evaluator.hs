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
           | TGridSet (Set Value) Value
           | TLambda (Map String Value) [String] Block
           deriving (Show, Eq, Ord)


parsedvars = Parser.parseAros "" "int myint = 5 + 3 * 2 ; vec thevec = <myint,2> ; {vec} place = {thevec} >> <3,3> ;  grid < 2 , 4 > , { <1,1> } routeRobot [ <1,1> ] "
parsedSimpleFunc = Parser.parseAros "" "( int -> int) myfunc = a -> { 2 * a } ; int b = myfunc (10) ; grid < 2 , 4 > , { <1,1> } routeRobot [<1,1>]"
parsedRec = Parser.parseAros "" "(int -> int) myfunc = a -> { if (a == 1) { 1 } else { a + myfunc ( a - 1 ) } } ; int b = myfunc (100) ;  grid < 2 , 4 > , { <1,1> } routeRobot [ <1,1> ] "
parsedCurry = Parser.parseAros "" "(int, int -> int) myfunc = (a,b) -> { a*b } ; (int -> int) mycurriedfunc = myfunc (2) ; int res = mycurriedfunc (210) ; grid < 2 , 4 > , { <1,1> } routeRobot [ <1,1> ] "


evalTree :: Either String Program -> Either String String
evalTree (Right (Program decls grid wpts)) = evaluateProgram Map.empty decls grid wpts
evalTree (Left _) = Left "err"

-- Parses definitions into a map, then calls handleRobot
evaluateProgram :: Map String Value -> [Declaration] -> GridDef -> RobotRoute -> Either String String
evaluateProgram defs ((Decl _ ident expr):xs) grd wpts =
  case (expHandler ident defs expr) of
    Right computedDecl -> evaluateProgram (Map.insert ident computedDecl defs) xs grd wpts
    Left e -> Left e
evaluateProgram defs [] grd wpts = handleRobot (handleGrid grd defs) wpts defs



handleGrid :: GridDef -> Map String Value -> Either String Value
handleGrid (GridDef e1 e2) defs = do
    playsize <- expHandler "" defs e1
    playmap <- expHandler "" defs e2
    case playmap of
      (TSet m) -> return $ TGridSet m playsize
      _ -> Left "Didn't get a Set"

testVexp :: Exp
testVexp = VectorExp (IntegerExp 69) (IntegerExp 420)
testListExp :: Exp
testListExp = ListExp [(IntegerExp 1), (IntegerExp 2), (IntegerExp 3)]
testSetExp :: Exp
testSetExp = SetExp [(IntegerExp 1), (IntegerExp 2), (IntegerExp 3)]

expHandler :: String -> Map String Value -> Exp -> Either String Value
expHandler _ defs (VariableExp ident) =
  case ( Map.lookup ident defs ) of
    (Just d) -> return d
    Nothing -> (Left $ "Lookup err - can't find " ++ ident ++ " in  " ++ show defs)
expHandler ident defs (ParenExp expr) = expHandler ident defs expr
expHandler _ _ (IntegerExp i) = Right $ TInt i
expHandler _ _ (BooleanExp b) = Right $ TBool b
expHandler _ defs (VectorExp a b) = do
    ua <- expHandler "" defs a
    ub <- expHandler "" defs b
    case (ua, ub) of
      ((TInt ia),(TInt ib)) -> return (TVec (ia, ib))
      _ -> Left $ "VectorExp err" ++ show a ++ " --- " ++ show b

expHandler _ defs (ListExp expList) = do
  mapped <- mapM ( expHandler "" defs ) expList
  return $ TList mapped

expHandler _ defs (SetExp expSet) = do
  mapped <- mapM (expHandler "" defs) expSet
  return $ TSet (Set.fromList mapped)

expHandler _ defs (BinaryExp exp1 bop exp2) = do
  e1 <- expHandler "" defs exp1
  e2 <- expHandler "" defs exp2
  binaryOperationHandler bop e1 e2

expHandler _ defs (UnaryExp uop expr) = do
  e <- expHandler "" defs expr
  unaryExpressionHandler uop e

expHandler identifier defs (LambdaExp strings _ block) = do
  return $ TLambda (Map.insert identifier (TLambda Map.empty [] (Block [] (IntegerExp 0))) defs) (declstringpairToStringHelper strings) block

expHandler identifier defs (ApplicationExp ident params) = do
  lambda <- expHandler "" defs ident
  let (VariableExp sident) = ident
  let (TLambda env paramNames block) = lambda
  let newenv = Map.insert sident lambda env
  curryHandler identifier newenv defs paramNames params block


expHandler _ defs (IfExp expr block1  block2) =
  let (Right (TBool evaluated)) = expHandler "" defs expr in
    if evaluated
      then blockHandler defs block1
      else blockHandler defs block2

expHandler _ defs (CondExp ((expr,block):xs) otherwiseBlock) =
  let (Right (TBool evaluated)) = expHandler "" defs expr in
    if evaluated
      then blockHandler defs block
      else expHandler "" defs (CondExp xs otherwiseBlock)
expHandler _ defs (CondExp [] otherwiseBlock) = blockHandler defs otherwiseBlock

blockHandler :: Map String Value -> Block -> Either String Value
blockHandler defs (Block ((Decl _ ident expr):xs) finalExp) =
  case (expHandler ident defs expr) of
    Left err -> Left $ "BlockHandler1 err: " ++ err
    Right handledExp -> blockHandler (Map.insert ident handledExp defs) (Block xs finalExp)
blockHandler defs (Block [] finalExp) = expHandler "" defs finalExp


curryHandler :: String -> Map String Value -> Map String Value -> [String] -> [Exp] -> Block -> Either String Value
curryHandler identifier closureenv origenv (x:xs) (y:ys) block = do
  evalled <- expHandler x origenv y
  let newenv = Map.insert x evalled closureenv
  curryHandler identifier newenv origenv xs ys block

curryHandler identifier closureenv _ e@(_:_) [] block =
  expHandler identifier closureenv (LambdaExp (map (\x -> (TypeInt, x)) e) TypeInt block) --HACK!!
curryHandler _ closureenv _ [] [] block = blockHandler closureenv block
curryHandler _ _ _ [] (_:_) _ = Left "Too many args to function"

declstringpairToStringHelper :: [(DeclType, String)] -> [String]
declstringpairToStringHelper ((_,s):xs) = s : declstringpairToStringHelper xs
declstringpairToStringHelper [] = []

et :: Either String String
et = evalTree parsedRec

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
binaryOperationHandler _ _ _ = Left "NoBop err"

unaryExpressionHandler :: UnaryOp -> Value -> Either String Value
unaryExpressionHandler Not (TBool e) = Right $ TBool $ not e
unaryExpressionHandler Head (TList (x:_)) = Right x
unaryExpressionHandler Tail (TList (_:xs)) = Right $ TList xs
unaryExpressionHandler Vecx (TVec (a,_)) = Right $ TInt a
unaryExpressionHandler Vecy (TVec (_,b)) = Right $ TInt b
unaryExpressionHandler _ _ = Left "NoUop err"


-- TODO
handleRobot :: Either String Value -> RobotRoute -> Map String Value -> Either String String
handleRobot (Right (TGridSet playmap playsize)) wpts defs = Right $ (show defs) ++ "  ------  " ++ (show $ Set.toList playmap) ++ " sized " ++ (show playsize)
handleRobot _ _ _ = Left "Robot err"
