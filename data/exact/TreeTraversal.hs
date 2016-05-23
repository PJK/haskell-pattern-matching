module TreeTraversal where

data Tree a = Fork a (Tree a) (Tree a) | Nil

func :: Tree a -> String
func Nil = "Empty tree"
func (Fork _ Nil Nil) = "Leaf"
-- At this point, we need correct propagation of value abstraction to handle this precisely, i.e.
-- deduce that that the last parameter can only be Fork
func (Fork _ Nil _) = "Right subtree only"
func (Fork _ _ Nil) = "Left subtree only"
-- Fork _ F F missing
