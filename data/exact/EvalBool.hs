module EvalBool where

func :: Bool -> Bool -> Int
func _ False = 1
func True False = 2
func _ _ = 3