module TreeEval where

data Tree
  = Leaf
  | Node Tree Tree


func' :: Tree -> Tree -> Int
func' _             Leaf = 1
func' (Node Leaf _) Leaf = 2
func' _             _    = 3
