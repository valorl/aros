-- |

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
           deriving (Eq,Show)


data Uop = Not | Head | Tail | Vecx | Vecy
         deriving (Eq, Show)

data Bop = Plus
  | Minus
  | Times
  | Div
  | Colon
  | PlusPlus
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
  | NEqual
  deriving (Eq, Show)


data Type = TypeInt
  | TypeVec
  | TypeBool
  | TypeList Type
  | TypeSet Type
  | TypeLambda [Type] Type
  | TypeLambdaNoParam Type
  deriving (Eq, Show)


data Declaration = Decl String Exp deriving Show

data Block = ABlock [Declaration] Exp
  deriving Show

data ExpBlock = Exp Block
  deriving Show

data Exp = Ident String
  | IntLit Int
  | BoolLit Bool
  | Vec Exp Exp
  | ListExp [Exp]
  | SetExp [Exp]
  | Bopped Exp Bop Exp
  | Uopped Uop Exp
  | LambdaExp [String] Block
  | FunctionAppl Exp [Exp]
  | IfExp Exp Block Block
  | CondExp [ExpBlock] Block
  deriving Show

data Program = Prog [Declaration] Exp Exp




