mul2and_ :: Integer -> Integer
mul2and_ x = x * 2 

--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

square' :: [Int] -> [Int]
square' xs = [x ^2 | x <- xs, x `mod` 2 == 0]

prod' :: [Int] -> Int
prod' xs = loop 1 xs
    where loop acc [] = acc
          loop acc (x:xs) = loop (x * acc) xs

add3to_ :: Integer -> Integer
add3to_ x = x + 3

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f = \x y -> f (x, y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f = \(x,y) -> f x y

cub' :: [Int] -> [Int]
cub' xs = [x ^3 | x <- xs, x `mod` 2 == 1]

selectEven :: Integral a => [a] -> [a]
selectEven xs = [x | x <- xs, x `mod` 2 == 0]

sumAbs :: [Int] -> Int
sumAbs [] = 0
sumAbs (x:xs) = if x > 0 then x + sumAbs xs else (-x) + sumAbs xs

filter p xs = [x | x <- xs, p x]

map f xs = [f x | x <- xs]
