
--import Control.Monad.Instances
import Data.Char

-- 14.2 Reader Monad

{-
     return x = \_ -> x
     h >>= f = \w -> f (h w) w
-}

addStuff = do
    a <- (* 2)
    b <- (+ 10)
    return (a + b)

addStuff' =
    (* 2) >>= (\a -> (+ 10) >>= \b -> return (a + b))


applyOp f op1 op2 = do
   a <- op1
   b <- op2
   return (f a b)

applyOp' f op1 op2 =
    op1 >>=  \a -> op2 >>=  \b -> return (f a b)

-- 中から
applyOp'' f op1 op2 =
    op1 >>= (\a -> \x ->   (\b -> return (f a b))  (op2 x) x)
applyOp''' f op1 op2 =
    \y ->   (\a -> \x ->   (\b -> \z ->  (f a b))  (op2 x) x) (op1 y) y
           -- a:(op1 y) x:y b:(op2 x) z:x
applyOp'''1 f op1 op2 =
    \y ->                        (\z ->  (f (op1 y) (op2 y)))  y

-- 外から
applyOp'''' f op1 op2 =
    \y ->   (\a -> op2 >>= (\b -> return (f a b))           ) (op1 y) y
applyOp''''' f op1 op2 =
    \y ->   (\a -> \x ->   (\b -> \z ->  (f a b))  (op2 x) x) (op1 y) y


testApOps f op1 op2 x =
    map fn [applyOp,applyOp',applyOp'',applyOp''',applyOp'''1, applyOp'''']
        where fn apop = apop f op1 op2 x

am = ((+ 5) >>=) 
aa = (+ 5) >>= (*)   -- \x -> (x + 5) * x
bb = (+) >>= \h x -> h (x * 2)
cc = (map toUpper) >>= \x -> (map toLower) >>= \y -> return (x ++ y)

main = do
    print $ addStuff 3
    print $ testApOps  (+) (*2) (+10) 3
    print $ applyOp'''1 (+) (*2) (+10) 3
    print $ applyOp'''1 (*) (*2) (+10) 3
    print $ applyOp'''1 (-) (*10) (+5) 3
    print $ applyOp' (++) (map toUpper) (map toLower) "abCdE"
    print $ map aa [1,2,3,4,5]
    print $ map bb [1,2,3,4,5]
    print $ cc "abCdE"
