import qualified Data.Char as Char

isPalindrome :: [Char] -> Bool
isPalindrome w = w == reverse w

describeLetter :: Char -> String
describeLetter c =
    if c >= 'a' && c <= 'z'
        then "Lower case"
        else if c >= 'A' && c <= 'Z'
            then "Upper case"
            else "Not an ASCII letter"

getElemAtIdx :: ([Char], Int) -> Char
getElemAtIdx (w, n) = if n > length w
  then '.'
    else head (drop n w)
-- where xs = drop (n-1) w
-- getElemAtIdx (w, n) = w !! n

--capitalize :: [Char] -> [Char]
--capitalize w = \

capitalized :: String -> String
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail
capitalized [] = []



