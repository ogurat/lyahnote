
import Control.Monad

-- 13.6 List Monad

{-
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []
-}

testListMonad =
    ([3,4,5] >>= \x -> [x,-x],
     [] >>= \x -> ["bad","mad","rad"])

l1 = [1,2] >>= \n -> ['a','b','c'] >>= \c -> return (n, c)

listOfTuples = do
    n <- [1,2]
    c <- ['a','b','c']
    return (n, c)

-- List comprehensions

listOf = [(n, c) | n <- [1,2], c <- ['a','b','c'] ] 

-- MonadPlus guard

l2 = [ x | x <- [1..50], '7' `elem` show x]


testgd =
    (guard (5 > 2) :: Maybe (),
     guard (1 > 2) :: Maybe (),
     guard (5 > 2) :: [()],
     guard (1 > 2) :: [()])
gd b = guard b >> return "cool" :: [String]
gd1 = (gd (5 > 2), gd (1 > 2))

l3 = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)

peval = ([3,4,5] >>=)
aa = peval (\x -> [x,x * 2,x * 3])
bb = peval (\x -> [x,x * 2,x * 3] >>= \y -> [x, y])

main = do
    print l1
    print l2
    print testgd
    print l3
    print aa
    print bb
