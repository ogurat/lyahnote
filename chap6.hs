
import Data.Char
import Data.List

numUniques :: Eq a => [a] -> Int
numUniques = length . nub
testUniq = (numUniques [1, 5, 1, 8, 9], numUniques ['a', 'b', 'b'], numUniques [True,True, False])

v2 = words "hey these are the words in this sentence"

wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

v3 = wordNums "wa wa wee wa wee"

needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

testisIn = (
    "art" `isIn` "party",
    [1,2] `isIn` [1,3,5])


digitSum = sum . map digitToInt . show
firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1..]

firstTo n = find (\x -> digitSum x == n) [1..]

v4 = map firstTo [27, 13, 40, 42]

-- 正格な左畳み込み

fl1  = foldl (+) 0  (replicate 100000000 1)
fl1' = foldl' (+) 0 (replicate 100000000 1)
