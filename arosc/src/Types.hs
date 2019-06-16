-- |


module Types where

data Type = TAny
  | TInteger
  | TBoolean
  | TFunction [Type] Type
  | TVector
  | TList Type
  | TSet Type
  deriving (Show)

instance Eq Type where
  (==) TInteger TInteger = True
  (==) TBoolean TBoolean = True
  (==) TVector TVector = True
  (==) (TList t1) (TList t2) = t1 == t2
  (==) (TSet t1) (TSet t2) = t1 == t2
  (==) (TFunction args1 out1) (TFunction args2 out2) = (args1 == args2) && (out1 == out2)
  (==) TAny _ = True
  (==) _ TAny = True
  (==) _ _ = False

selectNotAny :: Type -> Type -> Type
selectNotAny TAny t2 = t2
selectNotAny t1 TAny = t1
selectNotAny t1 t2 | t1 == t2 = t1
selectNotAny t1 t2 = error $ "Cannot select between " <> (show t1) <> " and " <> (show t2)
