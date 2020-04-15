
maximum' :: Ord a => [a] -> a
maximum' [x] = x
maximum' (x:xs) = x `max` (maximum' xs)

replicate' :: (Ord i, Num i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n - 1) x


take' n _
    | n <= 0    = []
take' _ [] = []
take' n (x:xs) = x:take' (n - 1) xs


reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let sq = [a | a <- xs, a <= x]
        l = [a | a <- xs, a > x]
    in quicksort sq ++ [x] ++ quicksort l

test_qs = quicksort [1, 5, 8, 5, 5, 4]
