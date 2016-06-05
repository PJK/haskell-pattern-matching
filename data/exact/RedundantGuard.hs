module RedundantGuard where


func :: Int -> Int
func x
    | x == 0 = 0
    | x > 0 = 1
    | x < 0 = -1
    | x == 3 = 3