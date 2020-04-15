
{-# LANGUAGE  NoMonomorphismRestriction #-}

import Data.Char
import Data.List
--import Control.Monad.Instances
import Control.Applicative


-- 11.1 Functors redux

abc = do
    line <- fmap reverse getLine
    putStrLn line

abcd = do
    line <- fmap (reverse . map toUpper) getLine
    putStrLn line


-- Functorとしての関数

{-
Prelude> :t fmap (* 3) (+ 100)
fmap (* 3) (+ 100) :: (Functor ((->) b), Num b) => b -> b
Prelude> :m Control.Monad.Instances
Prelude Control.Monad.Instances> :t fmap (* 3) (+ 100)
fmap (* 3) (+ 100) :: Num b => b -> b
-}


fn = fmap (* 3) (+ 100)
apfn = fn 1

-- lifting
{-
Prelude> :t fmap (* 2)
fmap (* 2) :: (Functor f, Num b) => f b -> f b
Prelude> :t fmap (replicate 3)
fmap (replicate 3) :: Functor f => f a -> f [a]
Prelude> :t fmap (++ "!!")
fmap (++ "!!") :: Functor f => f [Char] -> f [Char]
-}

-- :set -XNoMonomorphismRestriction
testlift =
    let f = fmap (* 2)  
    in (f [1,2,3,4],f (Right 4),f (Left "ha"),f (Just 10))

testlift1 =
    let f = fmap (replicate 3)  
    in (f [1,2,3,4],f (Just 3),f (Right "blah"),f Nothing,f (Left 5),f (Just "ha"))

testlift2 =
    let shout = fmap (++ "!!")
    in (shout ["ha","ka","ta"],shout (Right "blah"),shout (Left 5),shout (Just "ha"))

-- 11.3 Applicative Functor

test2 =
    let f1 = fmap (*) (Just 3) -- Justの中身が (3 *)
        f2 = fmap (++) (Just "hey")
        f3 = fmap compare (Just 'a')
        f4 = fmap compare "a list of chars"
        f5 = fmap (\x y z -> x * y + z) [3,4,5,6]
    in do
        print $ fmap (\f -> f 6) f1
        print $ fmap (\f -> f " joe") f2
        print $ fmap (\f -> f 'b') f3
        print $ fmap (\f -> f 'o') f4
        print $ fmap (\f -> f 10 5) f5  


test3 =
    let a = fmap (*) [1,2,3,4]
    in (fmap (\f -> f 9) a,fmap (\f -> f 2) a)


-- Maybe
{-
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> m = fmap f m
-}




test4 =
    (Just (+ 3) <*> Just 9,
     pure (+ 3) <*> Just 10,
     Just ("hahaha" ++) <*> Nothing,
     Just("hahaha" ++) <*> Just "!!",
     Nothing <*> Just "!!")


-- Applicative Style

test5 =
    (pure (+) <*> Just 3 <*> Just 5,
     pure (+) <*> Just 3 <*> Nothing,
     pure (+) <*> Nothing <*> Just 5)


test6 =
    ((++) <$> Just "john" <*> Just "travolta",
     (*) <$> Just 5 <*> Just 4,
     (++ "!!") <$> Just "hahaha")


-- list
{-
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
-}

test7 =  [(* 0),(+ 100),(2 ^)] <*> [1,2,3]
test7' = [(+),(*)] <*> [1,2] <*> [3,4]

test8 =  (*) <$> [1,2,3] <*> [4,5,6]
test8' = (++) <$> ["ha", "heh", "hmm"] <*> ["?","!","."]

test9 =
    ([x*y | x <- [2,5,4], y <- [8,10,11]],(*) <$> [2,5,4] <*> [8,10,11])

--　IOもApplicative

revup :: String -> String -> String
revup a b =
    (reverse a) ++ (map toUpper b)

stradd a b =
    read a + read b

test10 fn = fn <$> getLine <*> getLine

-- <$>と<*>を手動展開した同等
test11 fn = do
    r <- getLine
    f <- return (fn r)
    x <- getLine
    return (f x)

-- 関数もApplicative
{-
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
-}

test12 =
    (+) <$> (+ 3) <*> (* 100)

test13 f op1 op2 =
    f <$> op1 <*> op2

test13' f op1 op2 =  
    let f1 = f . op1 --関数Functorのfmapは関数合成
    in  f1 <*> op2

test13'' f op1 op2 =
    let f1 x = f (op1 x)
    in \x -> f1 x (op2 x)

test13''' f op1 op2 =
    \x -> f (op1 x) (op2 x)

test13'''' f op1 op2 x =
    f (op1 x) (op2 x)

testap f op1 op2 x =
    let fn test = test f op1 op2 x
    in map fn [test13,test13',test13'',test13''',test13'''']

testap' f op1 op2 x =
    map (\test -> test f op1 op2 x) [test13,test13',test13'',test13''',test13''''] 


-- zip list


zl1 = (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
zl2 = (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]
zl3 = max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]
test14 = map getZipList [zl1,zl2,zl3]


-- 11.4

liftA2' op ap1 ap2 =
    op <$> ap1 <*> ap2


sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs



test15 = sequenceA [Just 3,Just 2,Just 1]
test16 = sequenceA [Just 3,Nothing,Just 1]
test17 = sequenceA [Right 1,Right 2]
test18 = sequenceA [(+ 3),(+ 2),(+ 1)]
test19 = sequenceA [[1,2,3],[4,5]]
test20 = sequenceA [[1,2],[4,5,6]]
test21 = sequenceA [[1,2,3],[4,5,6],[3,3,5],[]]

ap7 = map (\f -> f 7) [(> 4),(< 10),odd]
apseq7 = sequenceA [(> 4),(< 10),odd] 7
testa = and apseq7

main = do
    test2
    print $ test12 5
    print $ testap (+) (+ 3) (* 100) 5
    print $ test13 (+) (+ 3) (* 100) 5
    print $ test13 (+) (+ 5) (* 2)   4
    print $ test18 3
    print $ test21
