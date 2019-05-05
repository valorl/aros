module Syntax where

-- TOKENS
data Token = TokenIntLit Int
           | TokenBoolLit Bool
           | TokenNot
           | TokenHead
           | TokenTail
           | TokenVecx
           | TokenVecy
           | TokenInt
           | TokenVec
           | TokenBool
           | TokenGrid
           | TokenRoute
           | TokenCrop
           | TokenAnd
           | TokenOr
           | TokenIf
           | TokenElse
           | TokenCond
           | TokenOtherwise
           | TokenArrow
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenColon
           | TokenDoublePlus
           | TokenUnion
           | TokenIntersection
           | TokenShift
           | TokenGte
           | TokenLte
           | TokenGt
           | TokenLt
           | TokenEq
           | TokenNeq
           | TokenAssign
           | TokenLParen
           | TokenRParen
           | TokenLBrace
           | TokenRBrace
           | TokenLBracket
           | TokenRBracket
           | TokenComma
           | TokenSemiColon
           | TokenIdent String
           | TokenEOF
           deriving (Eq,Show, Ord)


data UnaryOp = Not | Head | Tail | Vecx | Vecy
         deriving (Eq, Show, Ord)

data BinaryOp = Plus
  | Minus
  | Times
  | Div
  | Cons
  | Append
  | Union
  | Intersection
  | Shift
  | Crop
  | And
  | Or
  | Gt
  | Lt
  | Gte
  | Lte
  | Equal
  | NotEqual
  deriving (Eq, Show, Ord)


data DeclType = TypeInt
  | TypeVec
  | TypeBool
  | TypeList DeclType
  | TypeSet DeclType
  | TypeLambda [DeclType] DeclType
  deriving (Eq, Show, Ord)


data Declaration = Decl DeclType String Exp
  deriving (Eq, Show, Ord)

data Block = Block [Declaration] Exp
  deriving (Eq, Show, Ord)

data Exp = VariableExp String
  | ParenExp Exp
  | IntegerExp Int
  | BooleanExp Bool
  | VectorExp Exp Exp
  | ListExp [Exp]
  | SetExp [Exp]
  | BinaryExp Exp BinaryOp Exp
  | UnaryExp UnaryOp Exp
  | LambdaExp [(DeclType,String)] DeclType Block
  | ApplicationExp Exp [Exp]
  | IfExp Exp Block Block
  | CondExp [(Exp, Block)] Block
  deriving (Eq, Show, Ord)

data GridDef = GridDef Exp Exp
  deriving (Eq, Show, Ord)

data RobotRoute = RobotRoute Exp Exp
  deriving (Eq, Show, Ord)

data Program = Program [Declaration] GridDef RobotRoute
  deriving (Eq, Show, Ord)
