module Evaluator where
import Parser
import Syntax
import Data.Map

data TypedVariable = TInt Int
                   | TVec (Int, Int)
                   | TBool Bool
                   | TLInt [Int]
                   | TLVec [(Int, Int)]
                   | TSInt [Int]
                   | TSVec [(Int, Int)]
                   | TLambda
                   deriving Show


parsed :: Either String Program
parsed = Parser.parseAros "" "int myint = 5 ; grid < 2 , 4 > , { <1,1> } routeRobot [ <1,1> ] "

evalTree :: Either String Program -> Maybe String
evalTree (Right x) = evaluateProgram x Data.Map.empty
evalTree (Left _) = Nothing

-- Parses definitions into a map, then calls handleRobot
evaluateProgram :: Program -> Map String TypedVariable -> Maybe String
evaluateProgram (Program ((Decl dtype ident expr):xs) grd wpts ) defs =
  case (computeDecl dtype defs expr) of
    Nothing -> Nothing
    Just computedDecl -> evaluateProgram (Program xs grd wpts) (insert ident computedDecl defs)
evaluateProgram (Program [] grd wpts) defs = handleRobot (handleGrid grd defs) wpts defs


computeDecl :: DeclType -> Map String TypedVariable -> Exp -> Maybe TypedVariable
computeDecl dtype defs expr = handleExp defs expr

-- TODO
handleGrid :: GridDef -> Map String TypedVariable -> Maybe TypedVariable
handleGrid _ _ = Just (TSVec [ (1, 1), (2, 2), (3, 3) ])

vvv :: Exp
vvv = VectorExp (IntegerExp 69) (BooleanExp True)
vvv2 :: Exp
vvv2 = VectorExp (IntegerExp 69) (IntegerExp 420)
vvv3 :: Exp
vvv3 = ListExp [(IntegerExp 1), (IntegerExp 2), (IntegerExp 3)]

handleExp :: Map String TypedVariable -> Exp -> Maybe TypedVariable
handleExp defs (VariableExp ident) = Data.Map.lookup ident defs
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

handleExp defs (ListExp (x:xs)) =
  let
    handled = handleExp defs x
  in do
    (TInt ua) <- handled
    (TLInt ub) <- handleExp defs (ListExp xs)
    return (TLInt (ua : ub))

handleExp _ (ListExp []) = Just $ TLInt []


handleExp _ _ = Nothing



-- TODO
handleRobot :: Maybe TypedVariable -> [Exp] -> Map String TypedVariable -> Maybe String
handleRobot (Just (TSVec (x:xs))) wpts defs = Just (show defs)
handleRobot _ _ _ = Nothing
