module Variables where

data MyBool
    = True
    | False

f :: MyBool -> MyBool -> Integer
f True y = 4
f x y = 3