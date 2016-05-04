module Variables where

data MyBool
    = True
    | False

f :: MyBool -> MyBool -> Integer
f True y = 4
f x y = 3

incompleteF :: MyBool -> MyBool -> Integer
incompleteF True y = 4
incompleteF x False = 3