module FuncGuard where

isPrimeOrSmall :: Int -> Bool
isPrimeOrSmall x
    | isPrime x && x < 10 = True
    | not (isPrime x)        = False

