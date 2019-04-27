-- |
module Types where
import Data.Map(Map)
import Data.Set(Set)

data ConstType = TInteger
  | TBoolean
  | TVector
  | TList Type
  | TSet Type
  deriving (Eq, Show, Ord)

data Type = VarType !TypeVariable
  | FuncType ![Type] !Type
  | ListType Type
  | SetType Type
  | ConstType ConstType
  deriving (Eq, Show, Ord)


newtype TypeVariable = TypeVar Int
  deriving (Show, Eq, Ord)

type Name = String
type Environment = Map Name Type

type Constraint = (Type, Type)
type Constraints = Set Constraint
