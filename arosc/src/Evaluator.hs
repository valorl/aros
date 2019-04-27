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


handleExp _ _ = Nothing



-- TODO
handleRobot :: Maybe Value -> [Exp] -> Map String Value -> Maybe String
handleRobot (Just (TList (x:xs))) wpts defs = Just (show defs)
handleRobot _ _ _ = Nothing
