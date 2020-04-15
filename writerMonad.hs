
import Data.Monoid
import Control.Monad.Writer

-- 14.1 Writer? I hardly know her!

-- Writer type

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, l) = f x in (y, log `mappend` l)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jelly" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

testApplylog = do
    print $ (3, "Smallish gang.") `applyLog` \x -> (x > 9, "compared gang size to 9.")
    print $ (6, "Middle gang.")   `applyLog` \x -> (x > 5, "compared gang size to 5.")
    print $ ("Tobin", "got outlaw name.")   `applyLog` \x -> (length x, "applied length.")
    print $ ("Bathcat", "got outlaw name.") `applyLog` \x -> (length x, "applied length.")
    print $ ("beans", Sum 10) `applyLog` addDrink
    print $ ("jelly", Sum 25) `applyLog` addDrink
    print $ ("dogmeat", Sum 5) `applyLog` addDrink
    print $ ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink

{-
newtype Writer w a = Writer { runWriter :: (a, w) }
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    Writer (x, v) >>= f = let (Writer (y, v')) = f x 
                            in Writer (y, v `mappend` v')
-}

testrun =
    (runWriter (return 3 :: Writer String Int),
     runWriter (return 3 :: Writer (Sum Int) Int),
     runWriter (return 3 :: Writer (Product Int) Int))
                         
-- Using do notation with Writer

logNumber :: Int -> Writer [String] Int
logNumber n = writer (n, ["got number: " ++ show n])

multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    tell ["gonna multiply these two"]
    return (a * b)

multWithLog' = logNumber 3 >>= (\a -> logNumber 5 >>= \b -> return (a * b))

printWriter x = print (runWriter x)

testWriter = do
    printWriter $ writer (3, "Smallish gang.") >>= \x -> writer (x > 9, "compared gang size to 9.")
    printWriter $ writer (6, "Middle gang.")   >>= \x -> writer (x > 5, "compared gang size to 5.")
    printWriter $ writer ("Tobin", "got outlaw name.")   >>= \x -> writer (length x, "applied length.")
    printWriter $ writer ("Bathcat", "got outlaw name.") >>= \x -> writer (length x, "applied length.")
    printWriter $ writer ("beans", Sum 10) >>= writer . addDrink
    printWriter $ writer ("jelly", Sum 25) >>= writer . addDrink
    printWriter $ writer ("dogmeat", Sum 5) >>= writer . addDrink
    printWriter $ writer ("dogmeat", Sum 5) >>= writer . addDrink >>= writer . addDrink
    printWriter multWithLog
    printWriter multWithLog'

{-
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
     | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
     | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " show (a `mod` b)]
        gcd' b (a `mod` b)

testgcd = fst $ runWriter (gcd' 8 3)
testgcd2 = mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
-}

-- Difference lists

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))

difflistAppend = toDiffList [1,2,3,4] `mappend` toDiffList [5,6,7]

difftest = fromDiffList difflistAppend

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
    | b == 0 = do
        tell (toDiffList ["finished: " ++ show a])
	return a
    | otherwise = do
 	result <- gcd' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

ccc x =
  case runWriter x of
    (x, log) ->
       (fromDiffList log)

difflistgcd =
  map ccc [gcd' 110 34, gcd' 120 50, gcd' 20 8]

diffgcd =
    mapM_ print difflistgcd

countDown :: Int -> Writer (DiffList String) ()
countDown 0 = do
    tell (toDiffList ["0"])
countDown x = do
    countDown (x - 1)
    tell (toDiffList [show x])

testcountDown =
    mapM_ putStrLn . fromDiffList . snd . runWriter $ countDown 500000



countDown' :: Int -> Writer [String] ()
countDown' 0 = do
    tell ( ["0"])
countDown' x = do
    countDown' (x - 1)
    tell [show x]

testcountDown' =
    mapM_ putStrLn . snd . runWriter $ countDown' 500000


main = do
    testApplylog
    testWriter
    diffgcd
