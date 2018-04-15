import Data.List
import Data.Char

onlyEven [] = []
onlyEven (x:xs)
    | x `mod` 2 == 0 = x : onlyEven xs
    | otherwise = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
    | x `mod` 2 == 1 = x : onlyOdd xs
    | otherwise = onlyOdd xs

onlyUpper [] = []
onlyUpper (x:xs)
    | isUpper x == True = x : onlyUpper xs
    | otherwise = onlyUpper xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' p (x:xs)
    | p x       = x : filter'' p xs
    | otherwise = filter'' p xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [ x | x <- xs, p x]

onlyEven' xs  = filter' even xs
onlyOdd' xs  = filter' odd xs
onlyUpper' xs = filter' isUpper xs