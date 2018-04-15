absInt :: Int -> Int
absInt n | n >= 0 = n
        | otherwise = -n 

sgn :: Int -> Int
sgn n | n > 0 = n
    | n == 0 = 0
    | otherwise = -n

min3Int :: (Int, Int, Int) -> Int
min3Int (n,m,o) | n<m && n<o = n
                | m<n && m<o = m
                | otherwise = o