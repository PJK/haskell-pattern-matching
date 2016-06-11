module Tree where

data Tree a = Fork a (Tree a) (Tree a) | Nil

func :: Tree a -> Int
func Nil              = 1
func (Fork _ Nil Nil) = 2
func (Fork _ Nil _)   = 3
func (Fork _ _ Nil)   = 4
-- Fork _ F F missing
