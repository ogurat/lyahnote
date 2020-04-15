
-- 5.2 Some high-orderism is in order

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applytest = (applyTwice (+3) 10, applyTwice ("HAHA " ++) "HEY", applyTwice (3:) [1])

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zipa = zipWith' (+) [4,2,5,6] [2,6,2,3]
zipb = zipWith' max [6,3,2,1] [7,3,1,5]
zipc = zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
zipd = zipWith' (*) (replicate 5 2) [1..]
zipe = zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]

-- 5.3

-- map
mapa = map (+3) [1,5,3,1,6]
mapb = map (++ "!") ["biff", "bang", "pow"]
mapc = map (replicate 3) [3..6]
mapd = map (map (^2)) [[1,2],[3,4,5,6],[7,8]]

-- filter
num5_3 = [1,5,3,2,1,6,4,3,2,1]
filter1 = filter (>3) num5_3
filter2 = filter (==3) num5_3
filter3 = filter even [1..10]
filter4 = filter (\x -> not (null x)) [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
filter5 = filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
filter6 = filter (`elem` ['A'..'Z']) "i LAuGh at you bEcause u R all the same"
filter7 = filter (< 15) (filter even [1..20])
filter8 = [x | x <- [1..20], x < 1, even x] 

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let l = filter (<= x) xs
        r = filter (> x) xs
    in quicksort l ++ [x] ++ quicksort r

testqs = quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]

ff = takeWhile (/=' ') "elephants know how to party"

ss = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
ss2 = sum (takeWhile (<10000) [m | m <- [n^2 | n <-[1..]], odd m])

-- Collatz sequences
chain 1 = [1]
chain n
      | even n = n : chain (n `div` 2)
      | odd n  = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
                where isLong xs = length xs > 15

aa = zipWith (\a b -> (a*30 +3)/b) [5,4,3,2,1] [1,2,3,4,5]
ab = map (\(a,b) -> a + b) [(1,2), (3,5), (6,3), (2,6), (2,5)]

-- 5.5 

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0
sum'' xs = foldl (\acc x -> acc + x) 0 xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []
maptest = map' (+3) [1,2,3]

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max
maxtest = maximum' [1,3,2]

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- fold infinite list

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs


-- 5.6

sumsqrt = sum (map sqrt [1..130])
sumsqrt' = sum $ map sqrt [1..130]

a = sqrt ( 3 + 4 + 9)
a' = sqrt $ 3 + 4 + 9

b = sum (filter (> 10) (map (*2) [2..10]))
b' = sum $ filter (> 10) $ map (*2) [2..10]

c = map ($ 3) [(4+), (10*), (^2), sqrt]


-- 5.7

d = map (\x -> negate (abs x)) [5, 3, -6, 7, -3, 2, -19, 24]
d' = map (negate . abs) [5, 3, -6, 7, -3, 2, -19, 24]

e = map (\xs -> negate (sum (tail xs))) [[1..5], [3..6], [1..7]]
e' = map (negate . sum . tail) [[1..5], [3..6], [1..7]]

f = replicate 2 (product (map (*3) (zipWith max [1,2] [4,5])))
f' = replicate 2 . product . map (*3) $ zipWith max [1,2] [4,5]

fn x = ceiling (negate (tan (cos (max 50 x))))
fn' = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]

