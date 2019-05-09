module Evaluator where
import Parser
import Syntax
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq


data Value = TInt Int
           | TVec (Int, Int)
           | TBool Bool
           | TList [Value]
           | TSet (Set Value)
           | TGridSet (Set Value) Value
           | TLambda (Map String Value) [String] Block
           deriving (Show, Eq, Ord)


onlyGrid :: Either String Program
onlyGrid = Parser.parseAros "" "grid <10,10>, { <1,1>, <2,1> } routeRobot <0,0>, <9,9>"

et :: IO ()
et = putStr $ evalTree onlyGrid

-- start point, checks that program was evaluated ok
evalTree :: Either String Program -> String
evalTree (Right (Program decls grid wpts)) =
  case evaluateProgram Map.empty decls grid wpts of
    (Right x) -> x
    (Left err) -> err
evalTree (Left _) = "Program wasn't parsed correctly"

-- recurses through definitions, adding them to dict
-- then evaluates the robot route
-- finally pretty prints the info
evaluateProgram :: Map String Value -> [Declaration] -> GridDef -> RobotRoute -> Either String String
evaluateProgram defs ((Decl _ ident expr):xs) grd wpts =
  case (expHandler ident defs expr) of
    Right computedDecl -> evaluateProgram (Map.insert ident computedDecl defs) xs grd wpts
    Left e -> Left e
evaluateProgram defs [] grd wpts = do
  let (RobotRoute e1 e2) = wpts
  let (Right start@(TVec tstart)) = expHandler "" defs e1
  let (Right end@(TVec tend)) = expHandler "" defs e2
  let (Right (TGridSet playmap playsize@(TVec tplaysize))) = handleGrid grd defs
  resultPath <- handleRobot playmap playsize start end
  return $
    "map size " ++ show tplaysize ++ "\n" ++
    "obstacles " ++ (show $ map (\(TVec x) -> x) $ Set.toList playmap) ++ "\n" ++
    "start->end " ++ show tstart ++ "->" ++ show tend ++ "\n" ++
    resultPath ++ "\n"


------ ROBOT STUFF -----

-- list of (vec1,vec2), where vec1 is the path parent of vec2
-- knowing the final vec we can trace the route back to the beginning
-- starting vector has parent (-1,-1)
followParents :: (Eq a, Num a) => [((a,a),(a,a))] -> (a,a) -> [(a,a)]
followParents [] _ = []
followParents paths node
  | p == (-1,-1) = [node]
  | otherwise = node : followParents paths p
  where
    (p,_) = head $ filter (\(_,h) -> h == node) paths

-- self explanatory
cartesianProd :: [a] -> [b] -> [(a,b)]
cartesianProd xs ys = [ (a,b) | a <- xs, b <- ys ]

-- gives direction from vec1 to vec2
-- x up   -> down
-- y left -> right
directionToGoMaker :: (Ord a) => (a,a) -> (a,a) -> String
directionToGoMaker (x1,y1) (x2,y2)
  | x1 > x2 = "Up"
  | x1 < x2 = "Down"
  | y1 > y2 = "Left"
  | otherwise = "Right"

-- taking a list of coordinates (the path of the robot)
-- transforms it into a list of instructions
instructionsMaker :: (Ord a) => [(a,a)] -> String
instructionsMaker (x:y:xs) = directionToGoMaker x y ++ " " ++ instructionsMaker (y:xs)
instructionsMaker [_] = "Done."
instructionsMaker _ = "Empty."

-- creates a list of all vectors on the map, then removes the unavailable ones
-- from the remaining vectors we can make edges between neighboring vectors
-- finally computes and returns the fastest route for the robot
handleRobot :: (Set Value) -> Value -> Value -> Value -> Either String String
handleRobot playmap playsize (TVec start) (TVec end) = do
  let (TVec (x,y)) = playsize
  let allVecs = cartesianProd [0..(x-1)] [0..(y-1)]
  let obstacles = map (\(TVec vc) -> vc) $ Set.toList playmap
  let freeSquares = filter ( `notElem` obstacles ) allVecs
  let allEdges = filter ( \((x1,y1),(x2,y2)) -> (abs (x1 - x2) + abs (y1 - y2)) == 1 ) $ cartesianProd freeSquares freeSquares
  case pathRobot allEdges (Seq.empty Seq.|> ((-1,-1),start)) Set.empty end of
    (Right res) -> Right $ instructionsMaker $ reverse $ followParents res end
    (Left err) -> Left err
handleRobot _ _ _ _ = Left "Robot err"

-- using a fifo queue
-- pops the head
-- checks that it hasn't been already visited
-- puts the neighbors in the back of the queue with itself as parent
-- continues until finish is found (or queue empty)
-- path will later be found by tracing parents
pathRobot :: (Show a, Ord a) => [((a,a),(a,a))] -> Seq ((a,a),(a,a)) -> Set (a,a) -> (a,a) -> Either String [((a,a),(a,a))]
pathRobot graph fifo visited end
  | Seq.length fifo == 0 = Left "No path"
  | otherwise = do
    let h@(_,chead) = fifo `Seq.index` 0
    let restOfFifo = Seq.deleteAt 0 fifo
    if chead == end then
      Right $ [h]
      else if (chead `Set.member` visited) then
      pathRobot graph restOfFifo visited end
      else do
        let updatedVisited = Set.insert chead visited
        let toAddToFifo = map (\(_,y) -> y) $ filter (\(x,_) -> chead == x ) graph
        let updatedfifo = restOfFifo Seq.>< (Seq.fromList $ map (\x -> (chead,x)) toAddToFifo)
        result <- pathRobot graph updatedfifo updatedVisited end
        return $ h : result




------ EVALUATING STUFF ------


handleGrid :: GridDef -> Map String Value -> Either String Value
handleGrid (GridDef e1 e2) defs = do
    playsize <- expHandler "" defs e1
    playmap <- expHandler "" defs e2
    case playmap of
      (TSet m) -> return $ TGridSet m playsize
      _ -> Left "Didn't get a Set"


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
  let newenv = Map.insert x evalled (Map.union closureenv origenv)
  curryHandler identifier newenv origenv xs ys block

curryHandler identifier closureenv _ e@(_:_) [] block =
  expHandler identifier closureenv (LambdaExp (map (\x -> (TypeInt, x)) e) TypeInt block) --HACK!!
curryHandler _ closureenv _ [] [] block = blockHandler closureenv block
curryHandler _ _ _ [] (_:_) _ = Left "Too many args to function"

declstringpairToStringHelper :: [(DeclType, String)] -> [String]
declstringpairToStringHelper ((_,s):xs) = s : declstringpairToStringHelper xs
declstringpairToStringHelper [] = []

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
binaryOperationHandler a _ _  = Left $ "NoBop err on a " ++ show a ++ " operation"

unaryExpressionHandler :: UnaryOp -> Value -> Either String Value
unaryExpressionHandler Not (TBool e) = Right $ TBool $ not e
unaryExpressionHandler Head (TList (x:_)) = Right x
unaryExpressionHandler Tail (TList (_:xs)) = Right $ TList xs
unaryExpressionHandler Vecx (TVec (a,_)) = Right $ TInt a
unaryExpressionHandler Vecy (TVec (_,b)) = Right $ TInt b
unaryExpressionHandler _ _ = Left "NoUop err"




