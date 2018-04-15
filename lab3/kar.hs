import Data.Char

prodWith f [] = 1
prodWith f (x:xs) = f x * prodWith f xs                              --rekurencja

prodWith' f xs = product [f x | x<-xs]            --list comprehensions

prodWith'' f = foldr(\x acc-> f x * acc) 1   --foldr1

fun = sum.map((^2).length).filter(isUpper.head).words

fun1 = sum.map ( foldl1 ( \x y -> 2*x + y ) ).map ( map digitToInt ).filter ( all ( `elem` "01" ) ) .words