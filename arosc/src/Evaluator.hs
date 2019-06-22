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
           | TLambda (Map String Value) [(DeclType,String)] DeclType Block
           deriving (Show, Eq, Ord)

--                           Playsize   Obstacles     Start       End         Route
data EvalResult = EvalResult (Int, Int) [(Int, Int)] ((Int, Int), (Int, Int)) String


recCurried :: Either String Program
recCurried = Parser.parseAros "" "\
  \(int, int -> int) myfunc = (int a, int b) -> int {\
  \  if (b <= 1) { 1 }\
  \  else { b + myfunc ( a, b - a ) }\
  \};\
  \(int->int) curried = myfunc(1);\
  \int res = curried ( 100 );\
  \grid <10,10>, {<1,res>} route <0,1>, <8,8>"

et :: IO ()
et = do
  let (Right res) = evalTree recCurried
  putStr $ pprintResult res

pprintResult :: EvalResult -> String
pprintResult (EvalResult playsize obstacles (start, end) path) =
  "map size " ++ show playsize ++ "\n" ++
  "obstacles " ++ show obstacles ++ "\n" ++
  "start->end " ++ show start ++ "->" ++ show end ++ "\n" ++
  path ++ "\n"

-- start point, checks that program was evaluated ok
evalTree :: Either String Program -> Either String EvalResult
evalTree (Right (Program decls grid wpts)) =
  case evaluateProgram Map.empty decls grid wpts of
    (Right x) -> Right x
    (Left err) -> Left err
evalTree (Left _) = Left "Program wasn't parsed correctly"

-- recurses through definitions, adding them to dict
-- then evaluates the robot route
-- finally pretty prints the info
evaluateProgram :: Map String Value -> [Declaration] -> GridDef -> RobotRoute -> Either String EvalResult
evaluateProgram defs ((Decl _ ident expr):xs) grd wpts =
  case (expHandler defs expr) of
    Right computedDecl -> evaluateProgram (Map.insert ident computedDecl defs) xs grd wpts
    Left e -> Left e
evaluateProgram defs [] grd wpts = do
  let (RobotRoute e1 e2) = wpts
  let (Right start@(TVec tstart)) = expHandler defs e1
  let (Right end@(TVec tend)) = expHandler defs e2
  let (Right (TGridSet playmap playsize@(TVec tplaysize))) = handleGrid grd defs
  resultPath <- handleRobot playmap playsize start end
  return $ EvalResult tplaysize ( map (\(TVec x) -> x ) $ Set.toList playmap ) (tstart, tend) resultPath


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


handleRobot :: (Set Value) -> Value -> Value -> Value -> Either String String
handleRobot playmap (TVec mapsize) (TVec start) (TVec end) = do
  let obstacles = Set.map (\(TVec vc) -> vc) playmap
  case pathRobot mapsize obstacles (Seq.empty Seq.|> ((-1,-1),start)) end of
    (Right res) -> Right $ instructionsMaker $ reverse $ followParents res end
    (Left err) -> Left err
handleRobot _ _ _ _ = Left "Robot err"

-- using a fifo queue
-- pops the head
-- puts the neighbors in the back of the queue with itself as parent
-- continues until finish is found (or queue empty)
-- path will later be found by tracing parents
pathRobot :: (Show a, Ord a, Num a) => (a,a) -> Set (a,a) -> Seq ((a,a),(a,a)) -> (a,a) -> Either String [((a,a),(a,a))]
pathRobot mapsize@(mapx,mapy) obstacles fifo end
  | Seq.length fifo == 0 = Left "No path"
  | otherwise = do
    let h@(_,chead@(xc,yc)) = fifo `Seq.index` 0
    let restOfFifo = Seq.deleteAt 0 fifo
    if chead == end then
      Right $ [h]
    else do
      let updatedObstacles = Set.insert chead obstacles
      let toAddToFifo = filter (\n@(x,y) -> x>=0 && y>=0 && x<mapx && y<mapy && n `notElem` obstacles ) [(xc-1,yc),(xc+1,yc),(xc,yc-1),(xc,yc+1)]
      let updatedfifo = restOfFifo Seq.>< (Seq.fromList $ map (\x -> (chead,x)) toAddToFifo)
      result <- pathRobot mapsize updatedObstacles updatedfifo end
      return $ h : result




------ EVALUATING STUFF ------


handleGrid :: GridDef -> Map String Value -> Either String Value
handleGrid (GridDef e1 e2) defs = do
    playsize <- expHandler defs e1
    playmap <- expHandler defs e2
    case playmap of
      (TSet m) -> return $ TGridSet m playsize
      _ -> Left "Didn't get a Set"


expHandler :: Map String Value -> Exp -> Either String Value
expHandler defs (VariableExp ident) =
  case ( Map.lookup ident defs ) of
    (Just d) -> return d
    Nothing -> (Left $ "Lookup err - can't find " ++ ident ++ " in  " ++ show defs)
expHandler defs (ParenExp expr) = expHandler defs expr
expHandler _ (IntegerExp i) = Right $ TInt i
expHandler _ (BooleanExp b) = Right $ TBool b
expHandler defs (VectorExp a b) = do
    ua <- expHandler defs a
    ub <- expHandler defs b
    case (ua, ub) of
      ((TInt ia),(TInt ib)) -> return (TVec (ia, ib))
      _ -> Left $ "VectorExp err" ++ show a ++ " --- " ++ show b

expHandler defs (ListExp expList) = do
  mapped <- mapM ( expHandler defs ) expList
  return $ TList mapped

expHandler defs (SetExp expSet) = do
  mapped <- mapM (expHandler defs) expSet
  return $ TSet (Set.fromList mapped)

expHandler defs (BinaryExp exp1 bop exp2) = do
  e1 <- expHandler defs exp1
  e2 <- expHandler defs exp2
  binaryOperationHandler bop e1 e2

expHandler defs (UnaryExp uop expr) = do
  e <- expHandler defs expr
  unaryExpressionHandler uop e

expHandler defs (LambdaExp args retType block) = do
  return $ TLambda defs args retType block

expHandler defs (ApplicationExp ident params) = do
  lambda <- expHandler defs ident
  let (VariableExp sident) = ident
  let (TLambda env paramNames retType block) = lambda
  let newenv = Map.insert sident lambda env
  curryHandler newenv defs paramNames params retType block


expHandler defs (IfExp expr block1  block2) =
  let (Right (TBool evaluated)) = expHandler defs expr in
    if evaluated
      then blockHandler defs block1
      else blockHandler defs block2

expHandler defs (CondExp ((expr,block):xs) otherwiseBlock) =
  let (Right (TBool evaluated)) = expHandler defs expr in
    if evaluated
      then blockHandler defs block
      else expHandler defs (CondExp xs otherwiseBlock)
expHandler defs (CondExp [] otherwiseBlock) = blockHandler defs otherwiseBlock

blockHandler :: Map String Value -> Block -> Either String Value
blockHandler defs (Block ((Decl _ ident expr):xs) finalExp) =
  case (expHandler defs expr) of
    Left err -> Left $ "BlockHandler1 err: " ++ err
    Right handledExp -> blockHandler (Map.insert ident handledExp defs) (Block xs finalExp)
blockHandler defs (Block [] finalExp) = expHandler defs finalExp


curryHandler :: Map String Value -> Map String Value -> [(DeclType,String)] -> [Exp] -> DeclType -> Block -> Either String Value
curryHandler closureenv origenv ((_,x):xs) (y:ys) retType block = do
  evalled <- expHandler origenv y
  let newenv = Map.insert x evalled closureenv
  curryHandler newenv origenv xs ys retType block

curryHandler closureenv _ e@(_:_) [] retType block =
  return $ TLambda closureenv e retType block
curryHandler closureenv _ [] [] _ block = blockHandler closureenv block
curryHandler _ _ [] (_:_) _ _ = Left "Too many args to function"

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




