module Tree where

data Tree a = Fork a (Tree a) (Tree a) | Nil
