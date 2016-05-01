module Variables where

data MyBool
    = True
    | False

f :: MyBool -> MyBool -> Integer
f x y = 3