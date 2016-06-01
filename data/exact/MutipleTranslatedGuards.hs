module MutipleTranslatedGuards where

succ :: Int -> Int -> Bool
succ 0 1 = True
succ 1 2 = True
succ 2 3 = True
succ _ _ = False