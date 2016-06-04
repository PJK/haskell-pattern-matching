module ListUnzip where

unzip :: [a] -> [(a, a)]
unzip [] = []
unzip (x:y:zz) = (x, y):unzip zz
-- List of odd length missing

