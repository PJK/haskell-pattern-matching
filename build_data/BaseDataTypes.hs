module BaseDataTypes where

data Bool = False | True
data Ordering = LT | EQ | GT
type String = [Char]

data Maybe a = Nothing | Just a
  deriving (Eq, Ord)

data Either a b = Left a | Right b
  deriving (Eq, Ord, Read, Show, Typeable)

