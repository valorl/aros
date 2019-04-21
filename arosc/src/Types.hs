-- |


module Types where

data Type = TInteger
  | TBoolean
  | TFunction [Type] Type
  | TVector
  | TList Type
  | TSet Type
  deriving (Eq, Show)
