sgn :: Int -> Int
sgn n = if n < 0
    then -1
    else if n == 0
        then 0
        else 1

absInt :: Int -> Int
absInt n = if n < 0
    then -n
    else n

min2Int :: (Int, Int) -> (Int)
min2Int (n,m) = if n < m
    then n
    else m

min3Int :: (Int, Int, Int) -> (Int)
min3Int (n,m,o) = if n < m && n < o
    then n
    else if m < n && m < o
        then m
        else o

_min3Int :: (Int, Int, Int) -> Int
_min3Int (n,m,o) = min2Int (min2Int(n,m),o)
 