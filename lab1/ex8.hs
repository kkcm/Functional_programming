not' :: Bool -> Bool
not' b = case b of
    True -> False
    False -> True

absInt :: Int -> Int
absInt n = case (n>=0) of
    True -> n
    _ -> -n

and' :: (Bool, Bool) -> Bool
and' (a,b) = case (a && b) of
    True -> True
    _ -> False