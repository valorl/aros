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
           | TLambda
           deriving (Show, Eq, Ord)


parsed :: Either String Program
parsed = Parser.parseAros "" "int myint = 5 ; grid < 2 , 4 > , { <1,1> } routeRobot [ <1,1> ] "

evalTree :: Either String Program -> Maybe String
evalTree (Right x) = evaluateProgram x Map.empty
evalTree (Left _) = Nothing

-- Parses definitions into a map, then calls handleRobot
evaluateProgram :: Program -> Map String Value -> Maybe String
evaluateProgram (Program ((Decl dtype ident expr):xs) grd wpts ) defs =
  case (computeDecl dtype defs expr) of
    Nothing -> Nothing
    Just computedDecl -> evaluateProgram (Program xs grd wpts) (Map.insert ident computedDecl defs)
evaluateProgram (Program [] grd wpts) defs = handleRobot (handleGrid grd defs) wpts defs


computeDecl :: DeclType -> Map String Value -> Exp -> Maybe Value
computeDecl dtype defs expr = handleExp defs expr

-- TODO
handleGrid :: GridDef -> Map String Value -> Maybe Value
handleGrid _ _ = Just (TList [TVec (1, 1), TVec (2, 2), TVec (3, 3) ])

testVexp :: Exp
testVexp = VectorExp (IntegerExp 69) (IntegerExp 420)
testListExp :: Exp
testListExp = ListExp [(IntegerExp 1), (IntegerExp 2), (IntegerExp 3)]
testSetExp :: Exp
testSetExp = SetExp [(IntegerExp 1), (IntegerExp 2), (IntegerExp 3)]

handleExp :: Map String Value -> Exp -> Maybe Value
handleExp defs (VariableExp ident) = Map.lookup ident defs
handleExp defs (ParenExp expr) = handleExp defs expr
handleExp _ (IntegerExp i) = Just $ TInt i
handleExp _ (BooleanExp b) = Just $ TBool b
handleExp defs (VectorExp a b) =
  let
    ca = handleExp defs a
    cb = handleExp defs b
  in do
    (TInt ua) <- ca
    (TInt ub) <- cb
    return (TVec (ua, ub))

handleExp defs (ListExp expList) = do
  mapped <- mapM (handleExp defs) expList
  return $ TList mapped

handleExp defs (SetExp expSet) = do
  mapped <- mapM (handleExp defs) expSet
  return $ TSet (Set.fromList mapped)

handleExp defs (BinaryExp exp1 bop exp2) = do
  e1 <- handleExp defs exp1
  e2 <- handleExp defs exp2
  case bop of
    Plus -> intOperation bop e1 e2
    Minus -> intOperation bop e1 e2
    Times -> intOperation bop e1 e2
    Div -> intOperation bop e1 e2
    Cons -> consOperation e1 e2
-- ?    Append ->
    Union -> setsOperation bop e1 e2
    Intersection -> setsOperation bop e1 e2
    Shift -> setVecOperation bop e1 e2
    Crop -> setVecOperation bop e1 e2
    And -> booleanOperation bop e1 e2
    Or -> booleanOperation bop e1 e2
    Gt -> comparativeOperations bop e1 e2
    Lt -> comparativeOperations bop e1 e2
    Gte -> comparativeOperations bop e1 e2
    Lte -> comparativeOperations bop e1 e2
    Equal -> comparativeOperations bop e1 e2
    NotEqual -> comparativeOperations bop e1 e2
    _ -> Nothing

handleExp _ _ = Nothing

intOperation :: BinaryOp -> Value -> Value -> Maybe Value
intOperation Plus  (TInt i) (TInt j) = Just $ TInt $ i+j
intOperation Minus (TInt i) (TInt j) = Just $ TInt $ i-j
intOperation Times (TInt i) (TInt j) = Just $ TInt $ i*j
intOperation Div   (TInt i) (TInt j) = Just $ TInt $ div i j
intOperation _ _ _ = Nothing

consOperation :: Value -> Value -> Maybe Value
consOperation i (TList xs)  = Just $ TList $ i:xs
consOperation _ _ = Nothing

setsOperation :: BinaryOp -> Value -> Value -> Maybe Value
setsOperation Union (TSet s1) (TSet s2) = Just $ TSet $ Set.union s1 s2
setsOperation Intersection (TSet s1) (TSet s2) = Just $ TSet $ Set.intersection s1 s2
setsOperation _ _ _ = Nothing


setVecOperation :: BinaryOp -> Value -> Value -> Maybe Value
setVecOperation Shift (TSet s) (TVec (a,b)) = Just $ TSet $ Set.map (\(TVec (x,y)) -> TVec (x+a, y+b)) s
setVecOperation Crop (TSet s) (TVec (a,b)) =  Just $ TSet $ Set.filter (\(TVec (x,y)) -> x<=a && y<=b) s
setVecOperation _ _ _ = Nothing


booleanOperation :: BinaryOp -> Value -> Value -> Maybe Value
booleanOperation And (TBool b1) (TBool b2) = Just $ TBool $ b1 == b2
booleanOperation Or (TBool b1) (TBool b2) = Just $ TBool $ b1 || b2
booleanOperation _ _ _ = Nothing


comparativeOperations :: BinaryOp -> Value -> Value -> Maybe Value
comparativeOperations bop (TBool b1) (TBool b2) =
  case bop of
    Equal -> Just $ TBool $ b1 == b2
    NotEqual -> Just $ TBool $ b1 /= b2
    _ -> Nothing
comparativeOperations bop (TInt i) (TInt j) =
  case bop of
    Equal -> Just $ TBool $ i == j
    NotEqual -> Just $ TBool $ i /= j
    Gt -> Just $ TBool $ i > j
    Lt -> Just $ TBool $ i < j
    Gte -> Just $ TBool $ i >= j
    Lte -> Just $ TBool $ i <= j
    _ -> Nothing
comparativeOperations bop (TVec (i1,j1)) (TVec (i2,j2)) =
  case bop of
    Equal -> Just $ TBool $ ( i1 == j1 ) && ( i2 ==  j2)
    NotEqual -> Just $ TBool $  ( i1 /= j1 ) || ( i2 /=  j2)
    Gt -> Just $ TBool $  ( i1 > j1 ) && ( i2 >  j2)
    Lt -> Just $ TBool $  ( i1 < j1 ) && ( i2 <  j2)
    Gte -> Just $ TBool $  ( i1 >= j1 ) && ( i2 >=  j2)
    Lte -> Just $ TBool $  ( i1 <= j1 ) && ( i2 <=  j2)
    _ -> Nothing
comparativeOperations _ _ _ = Nothing



-- TODO
handleRobot :: Maybe Value -> [Exp] -> Map String Value -> Maybe String
handleRobot (Just (TList (x:xs))) wpts defs = Just (show defs)
handleRobot _ _ _ = Nothing
