import Data.Char


doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs

sqrElems []     = []
sqrElems (x:xs) = x^2 : sqrElems xs

lowerCase [] = []
lowerCase (x:xs) = toLower x : lowerCase xs

map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [ f x | x <- xs]

doubleElems' xs = map' (\e -> e*2) xs 
sqrElems'    xs = map' (\e -> e^2) xs
lowerCase'   xs = map' (\e -> toLower e) xs