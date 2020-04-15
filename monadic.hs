
{- LANGUAGE NoMonomorphismRestriction -}

import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

import Data.Char
import Stack

-- 14.5 Some useful monadic functions

-- liftM


liftM' f m = m >>= (\x -> return (f x))

{-

stst =  runState (liftst (+ 5) pop)
ststm = runState (liftM' (+ 5) pop)

-}

liftSq :: (Monad m, Num r) => m r -> m r
liftSq = liftM (\x -> x * x)

fmapSq :: (Functor f, Num a) => f a -> f a
fmapSq = fmap (\x -> x * x)

l0 = liftSq [1,2,3,4,5,6]
l1 = liftSq (Just 8)
l11= liftSq Nothing
l12= liftSq (Right 8)
l13= liftSq pop
f1 = fmapSq (Just 8)
f11= fmapSq Nothing

l2 = runWriter $ liftM not $ writer (True, "chickpeas")
f2 = runWriter $ fmap  not $ writer (True, "chickpeas")

l2' = runWriter $ liftSq $ writer (5, "chickpeas")
f2' = runWriter $ fmapSq $ writer (5, "chickpeas")

l3 = runState (liftSq pop) [2,3,4,5,6]
f3 = runState (fmapSq pop) [2,3,4,5,6]
lnp = liftM not pop

l4 = liftSq (+ 5)  -- \x -> a * a where a = (x + 5) 
f4 = fmapSq (+ 5)
l5 = liftM (*) (+ 5)


printlifte f m =
    fn (liftM f m)
    where fn :: Show a => Either String a -> IO ()
          fn = print

printe :: (Show a) =>Either String a -> IO ()
printe = print

testLift = do
    print $ liftM (\x -> x * x) (Just 5)
    print $ liftM (\x -> x * x) Nothing
    print $ liftM (\x -> x * x) [3,4,5,6,7]
    print $ runWriter (liftM (\x -> x * x) (writer (3, "x log")))
    print $ liftM (* 2) (+ 5) 2
    print $ runState (liftM (+ 2) pop) [5,6,7,8]
    printe $ liftM (+ 2) (Right 5)
    printe $ liftM (\x -> x * x) (Right 3)
    printe $ liftM (++ "!") (Right "asd")
    printe $ liftM (\x -> x * x) (Left "err!")
    print l0
    print l1
    print f1
    print l2
    print f2
    print l3
    print $ runState lnp [False, True, False]
    print f3
    print (l4 3)
    print (f4 3)
    print (l5 4 5)
    --print (r1 4)
    --print (r2 4 5)
    

-- ap

ap' mf m = mf >>=  \f ->  m >>=  \x -> return (f x)
ap'' mf m = mf >>= \f -> liftM f m

{-

appp = runState ((state $ \t -> (\x -> x*x, t ++ "!")) `ap` (state $ \x -> (8, x ++ "?")))  "cc"

apap = runState (pop `ap` (state $ \ (x:xs) -> (x 10,xs))) [(+ 5),(* 6)]
apap' = let (a, s) = apap in a



-}

wap =  runWriter $ (writer ((+ 3), "xxxx")) `ap` (writer (5, "yyyy"))


--a1 =  (Just (+ 3) <*>)
--ap1 = (Just (+ 3) `ap`)
--ap1'= (Just (+ 3) `ap'`)
--am1 = (Just (+ 3) `apm`)



--a3  = (*) <*> (+ 5)
--ap3 = (*) `ap` (+ 5)  -- \x -> x * (x + 5)
--ar3 = (*) `apr5` (+ 5)

{-
printape mf m =
    fn (ap mf m)
    where fn :: (Show a) => Either String a -> IO ()
          fn = print
-}

--printape :: (Show a) =>Either String a -> IO ()
--printape = print

testAp = do
    print $ ap (*) (+ 5) 5
    print $ ap (Just (+ 3)) (Just 4)
    print $ ap (Just (+ 3)) Nothing
    print $ ap [(+ 1),(+ 2)] [5,6,7]
    print $ ap [(+ 1),(+ 2),(+ 3)] [2,5]
    print $ runWriter (ap (writer ((+ 3), "a")) (writer (5, "sd")))
    print $ runState (ap (state $ \t -> (map toUpper, t + 1)) (state $ \x -> ("asd", x + 1))) 10
    printe $ ap (Right (+ 5)) (Right 3)
    printe $ ap (Right ('a' :)) (Right "asd")
    printe $ ap (Right (+ 5)) (Left "err!")
    print  wap
    --mapM_ (\f -> (print . f) (Just 5)) [a1,ap1]

    mapM_ ( \x -> print $ (ap (*) (+ 5) x)) [3,4,5,6,7]


-- join

join' mm = mm >>= \m -> m
{-

je = joine (Right (Right 9)) -- :: Num a => Either String a

jw = joinw (writer (writer (1, "aaaa"), "bbb"))

-}

jw = join (writer (writer (1, "aaaa"), "bbb"))


printjoinm mm =
     fn (join mm)
     where fn :: (Show a) => Maybe a -> IO ()
           fn = print
printjoine mm =
     fn (join mm)
     where fn :: Either String Int -> IO ()
           fn = print

printje :: Either String Int -> IO ()
printje = print

testJoin = do
    print $ runWriter jw
    print $ join (Just (Just 9))
    print $ (join (Just Nothing) :: Maybe Int)
    print $ (join Nothing :: Maybe Int)
    print $ join [[1,2],[4,5,6]]
    print $ join (\x -> \y -> x ++ y) "abc"
    printje $ join (Right (Right 8))
    printje $ join (Right (Left "eerr!"))
    printje $ join (Left "err!")
    print $ runState (join (state $ \s -> (pop, 1:2:s))) [4,5,6]


-- filterM

keepSmall x
    | x < 4 = do
        tell ["keeping:" ++ show x]
	return True
    | otherwise = do
        tell [show x ++ " is throwing it away"]
	return False

fil = filterM keepSmall [9,1,5,3]
powerset = filterM (\x -> [True, False])

minusNo x =
    if x < 0 
        then Nothing
        else if x < 10
            then Just True
            else Just False


-- foldM

preds acc x
    | x > 9 =     Nothing
    | otherwise = Just (acc + x)

testFilter = do
    print $ runWriter fil
    print $ powerset [1,2,3]
    print $ pl [7,8,9,10,11,15]
    print $ pl [8,9,10,11]
    print $ pl [4,5, (-1)]
    print $ foldM preds 0 [2,8,3,1]
    print $ foldM preds 0 [2,11,3,1]
    where pl = filterM minusNo


--f <=< g = (\x -> g x >>= f)

g :: (Monad m, Num a) => a -> m a
g = (\x -> return (x + 1)) <=< (\x -> return (x * 100))

g1 = g 4 :: Maybe Int
g2 = g 4 :: [Int]
g3 = Just 4 >>= g
g4 = [4] >>= g
g5 = (+ 2) >>= g
testg = do
    print g1
    print g2
    print g3
    print g4
    print (g5 3)

main = do
    testLift
    testAp
    testJoin
    testFilter
    testg
