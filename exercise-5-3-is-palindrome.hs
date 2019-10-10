isPalindrome :: String -> Bool

isPalindrome "" = True
isPalindrome [h] = True
isPalindrome (h:t) = h == (last t) && isPalindrome (init t)
