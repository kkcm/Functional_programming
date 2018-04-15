myFun x = 2*x

add2T :: Num a => (a,a) -> a
add2T (x,y) = x + y

add2C :: Num a => a -> (a -> a)
add2C x y = x + y

add3T :: Num a => (a,a,a) -> a
add3T (x,y,z) = x + y + z

add3C :: Num a => a -> (a -> (a -> a))
add3C x y z = x + y + z

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f = \x y -> f (x, y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f = \(x,y) -> f x y

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f = \x y z -> f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f = \(x, y, z) -> f x y z

fiveToPower_ :: Integer -> Integer
fiveToPower_ n = (5 ^) n

_ToPower5 :: Num a => a-> a
_ToPower5 n = (^ 5) n

substrNFrom5 :: Num a => a -> a
substrNFrom5 n = (5 -) n

substr5From_ :: Num a => a -> a
substr5From_ n = (-) n 5

flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f = \ x y -> f y x

flip3 :: (a -> b -> c -> d) -> (c -> b -> a -> d)
flip3 f = \ x y z -> f z y x
