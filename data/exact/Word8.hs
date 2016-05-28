-- Exhaustive, but our solver won't figure that out.
f :: Word8 -> Int
f x | x >= 0 && x < 256 = 1

