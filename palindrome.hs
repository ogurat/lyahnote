
-- 9.1

respondPalindromes :: String -> String
respondPalindromes = unlines . mapPal . lines
  where
    mapPal = map (\xs -> if isPal xs then "palindrome" else "not a palindrome")

isPal xs = xs == reverse xs

main = interact respondPalindromes
