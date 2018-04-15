sqr x = x^2

funcFactory n = case n of
    1 -> id
    2 -> sqr
    3 -> (^3)
    4 -> \x -> (x^4)
    5 -> intFunc
    _ -> const n
    where
        intFunc x = x^5

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = case n of
    0 -> const 1
    1 -> (\e -> 1 + (e / (fact 1)))
    2 -> (\e -> (expApproxUpTo 1 e) + ((e^2)/(fact 2)))
    3 -> (\e -> (expApproxUpTo 2 e) + ((e^3)/(fact 3)))
    4 -> (\e -> (expApproxUpTo 3 e) + ((e^4)/(fact 4)))
    5 -> (\e -> (expApproxUpTo 4 e) + ((e^5)/(fact 5)))
    6 -> (\e -> (expApproxUpTo 5 e) + ((e^6)/(fact 6)))
    y -> (\e -> (expApproxUpTo (y-1) e) + ((e^y)/(fact y)))
    where
        fact n = fromIntegral $ product [1..n]

expApproxUpTo' :: Int -> Double -> Double
expApproxUpTo' n = (\e -> 1 + (sum [(e^k) / (fromIntegral $ product [1..k]) | k <- [1..n]]))