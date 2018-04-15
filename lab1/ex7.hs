not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
itItTheAnswer "Love" = True
isItTheAnswer _ = False

or' :: (Bool, Bool) -> Bool
or' (True, True) = True
or' (True, False) = True
or' (False, True) = True
or' (False, False) = False