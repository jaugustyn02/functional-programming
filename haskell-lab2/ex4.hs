isPalindrome :: [Char] -> Bool
isPalindrome s = s == (reverse s)

getElemAtIdx :: [Char] -> Int -> Char
getElemAtIdx s i = last (take i s)

capitalize :: [Char] -> [Char]
capitalize w = (toEnum(fromEnum(head w) - 32)::Char) : (drop 1 w)