isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

--isPrime2 :: Integral t => t -> Bool 
--isPrime2 n = if last eratoSieve [2..n] == n
--    then True
--    else False

--allEqual :: Eq a => [a] -> Bool
--allEqual [] = True
--allEqual (x:xs) = if x == head xs
  --  then allEqual xs 
  --  else False 
--allEqual xs = if all (==head xs) (tail xs) then True else False