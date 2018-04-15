fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

Div2 :: Eq a => [a] -> Bool
Div2 (x : y : _) | (y `mod` x) == 0 = True
Div2 _                              = False