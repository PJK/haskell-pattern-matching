module ListUnzip where

unzip :: [a] -> [(a, a, a)]
unzip [] = []
unzip (x:y:z:zz) = (x, y, z):unzip zz
-- List of length 1 and 2 missing

