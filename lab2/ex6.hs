fib :: (Num a, Eq a) => a -> a
fib n =
    if n == 0 || n == 1 then n
    else fib (n-2) + fib (n-1)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = if x 
    then True
    else or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = if x
    then and' xs
    else False

elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) = if x == n
    then True
    else elem' n xs

doubleAll :: Num t => [t] -> [t]
doubleAll xs = [x * 2 | x <- xs]

squareAll :: Num t => [t] -> [t]
squareAll xs = [x ^2 | x <- xs]

selectEven :: Integral t => [t] -> [t]
selectEven xs = [x | x <- xs, x `mod` 2 == 0]

average' ::[Int] -> Int
average' xs = (sum' xs) `div` (length xs)