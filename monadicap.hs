
import Control.Monad
import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Data.Char
import Stack

-- 14.5

-- ap

ap' mf m = mf >>=  \f ->  m >>=  \x -> return (f x)
ap'' mf m = mf >>= \f -> liftM f m


apr  mf m = \w -> (\f ->  m >>= (\x -> return (f x)))  (mf w) w  -- \xを囲むカッコは省略可能

apr1 mf m = \w ->        (m >>= (\x -> return ((mf w) x)))  w  
apr2 mf m = \w ->        (\v -> (\x -> return ((mf w) x))  (m v) v) w
apr3 mf m = \w ->               (\x -> return ((mf w) x))  (m w) w
apr4 mf m = \w ->                     (return ((mf w) (m w))) w
apr5 mf m = \w ->                     (\_ ->  ((mf w) (m w))) w

apr6 mf m = \w -> (\f -> (\v -> (\x -> return (f x))       (m v) v))  (mf w) w
apr7 mf m = \w ->        (\v -> (\x -> return ((mf w) x))  (m v) v) w
apr8 mf m = \w ->               (\x -> return ((mf w) x))  (m w) w
apr9 mf m = \w ->                     (return ((mf w) (m w))) w
apr10 mf m = \w ->                    (\_ ->  ((mf w) (m w))) w


mapapr mf m x =
    map (\a -> a mf m x) [(<*>),ap, ap',apr,apr1,apr2,apr3,apr4,apr5,apr6]



apm (Just h) m  = (\f -> m >>= \x -> return (f x))  h
apm Nothing _ = Nothing
apm1 (Just h) m =      (m >>= \x -> return (h x))
apm1 Nothing m  = Nothing
apm2 (Just h) (Just b) =     (\x -> Just (h x))  b
apm2 _ _ = Nothing
apm3 (Just h) (Just b) =           (Just (h b))
apm3 _ _ = Nothing
{-
apm2' (Just h) (Just b) = (\x -> Just (h x))  b
apm2' _  _  = Nothing
-}

mapapm mf m =
    map (\x -> x mf m) [(<*>),ap,ap',apm,apm1,apm2,apm3]


apl  mf m = concat (map (\f -> m >>= \x -> return (f x)) mf)
apl1 mf m = concat (map (\f -> concat (map (\x-> return (f x)) m)) mf)
apl2 mf m = concat (map (\f -> concat (map (\x-> [f x]) m)) mf)

mapapl mf m =
    map (\x -> x mf m) [(<*>),ap,ap',apl,apl1,apl2]


apw wh m = let (h,v) = runWriter wh
               (y,v') = runWriter ((\f -> m >>= \x -> return (f x)) h)
               in writer (y,v `mappend` v')

apw1' wh m = let (h,v) = runWriter wh
                 (y,v') = runWriter (m >>= \x -> return (h x))
                 in writer (y,v `mappend` v')

apw2' wh m = let (h,v) = runWriter wh; (xx,v2) = runWriter m
                 (y,v') = runWriter (let (y2,v2') = runWriter ((\x -> return (h x)) xx)
                                     in writer (y2, v2 `mappend` v2'))
                 in writer (y,v `mappend` v')

apw3' wh m = let (h,v) = runWriter wh; (xx,v2) = runWriter m
                 (y,v') = runWriter (let (y2,v2') = runWriter (return (h xx))
                                     in writer (y2,v2 `mappend` v2'))
                 in writer (y,v `mappend` v')

apw4' wh m = let (h,v) = runWriter wh; (xx,v2) = runWriter m
                 (y,v') = runWriter (let (y2,v2') = runWriter (writer ((h xx),mempty))
                                     in writer (y2,v2 `mappend` v2'))
                 in writer (y,v `mappend` v')

apw5' wh m = let (h,v) = runWriter wh; (xx,v2) = runWriter m
                 (y,v') = runWriter (writer ((h xx),v2 `mappend` mempty))
                 in writer (y,v `mappend` v')

apw6' wh m = let (h,v) = runWriter wh; (xx,v2) = runWriter m
                 in writer ((h xx),v `mappend` (v2 `mappend` mempty))


mapapw mf m =
    let fn test = runWriter (test mf m) 
    in map fn [ap,ap',apw,apw1',apw2',apw3',apw4',apw5',apw6']


apst sh m = let h = runState sh
    in state $ \s -> let (a,s') = h s
                         g = runState ((\f -> m >>= \x -> return (f x)) a)
            in g s'
apst1 sh m = let h = runState sh
    in state $ \s -> let (f,s') = h s
                         g = runState (m >>= \x -> return (f x))
            in g s'
apst2 sh m = let h = runState sh; h' = runState m
    in state $ \s -> let (f,s') = h s
                         g = runState (state $ \ss ->
                             let (a',s'') = h' ss
                                 g' = runState ((\y -> return (f y)) a')
                             in g' s'')
            in g s'
apst3 sh m = let h = runState sh; h' = runState m
    in state $ \s -> let (f,s') = h s
                         g = \ss ->
                             let (a',s'') = h' ss
                                 g' = runState ((\y -> return (f y)) a')
                             in g' s''
            in g s'
apst4 sh m = let h = runState sh; h' = runState m
    in state $ \s -> let (f,s') = h s
                         g = \ss ->
                             let (a',s'') = h' ss
                                 g' = runState (return (f a'))
                             in g' s''
                    in g s'
apst5 sh m = let h = runState sh; h' = runState m
    in state $ \s -> let (f,s') = h s
                         g = \ss ->
                             let (a',s'') = h' ss
                                 g' = runState (state (\sss -> ((f a'),sss)))
                             in g' s''
                     in g s'
apst6 sh m = let h = runState sh; h' = runState m
    in state $ \s -> let (f,s') = h s
                         g = \ss ->
                             let (a',s'') = h' ss
                                 g' = (\sss -> ((f a'),sss))
                             in g' s''
                     in g s'
apst7 sh m = let h = runState sh; h' = runState m
    in state $ \s -> let (f,s') = h s
                         g = \ss ->
                             let (a',s'') = h' ss
                             in ((f a'),s'')
                     in g s'
apst8 sh m = let h = runState sh; h' = runState m
    in state $ \s -> let (f,s') = h s
                     in let (a',s'') = h' s'
                        in ((f a'),s'')

appp = runState ((state $ \t -> (\x -> x*x, t ++ "!")) `ap` (state $ \x -> (8, x ++ "?")))  "cc"

apap = runState (pop `ap` (state $ \ (x:xs) -> (x 10,xs))) [(+ 5),(* 6)]
apap' = let (a, s) = apap in a

mapapst mf m s =
    let fn test = runState (test mf m) s 
    in  map fn [ap,ap',apst,apst1,apst2,apst3,apst4,apst5,apst6,apst7,apst8]


ape (Right h)  m  = (\f -> m >>= \x -> return (f x))  h
ape (Left err) _  = Left err
ape1 (Right h) m  =      (m >>= \x -> return (h x))
ape1 (Left err) _ = Left err
ape2 (Right h) (Right b) =    (\x -> return (h x))  b
ape2 (Right h) (Left err) = Left err
ape2 (Left err) _ = Left err
ape3 (Right h) (Right b) =           (Right (h b))
ape3 (Right h) (Left err) = Left err
ape3 (Left err) _ = Left err
{-
ape2' (Right h) m =
    case m of
        Right b  -> (\x -> Right (h x))  b
        Left err -> Left err
ape2' (Left err) _ = Left err
-}

appe = (Right (+ 5)) `ap` (Right 3)

mapape mf m  =
    map (\x -> x mf m) [ap,ap',ape,ape1,ape2,ape3]

a1 =  (Just (+ 3) <*>)
ap1 = (Just (+ 3) `ap`)
ap1'= (Just (+ 3) `ap'`)
am1 = (Just (+ 3) `apm`)
am3 = (Just (+ 3) `apm3`)

a3  = (*) <*> (+ 5)
ap3 = (*) `ap` (+ 5)  -- \x -> x * (x + 5)
ar3 = (*) `apr5` (+ 5)
{-
printmapape mf m =
    mapM_ fn (mapape mf m)
    where fn :: (Show a) => Either String a -> IO ()
          fn = print
-}
printmapapr mf m xs =
    mapM_ (\x -> print (mapapr mf m x)) xs


printe :: Show a => [Either String a] -> IO ()
printe = print
{-
printapr :: (Num [t], Num t) => [[t]] -> IO ()
printapr = print
-}
testAp = do
    print $ mapapr (*) (+ 5) 5
    print $ mapapm (Just (+ 3)) (Just 4)
    print $ mapapm (Just (+ 3)) Nothing
    print $ mapapl [(+ 1),(+ 2)] [5,6,7]
    print $ mapapl [(+ 1),(+ 2),(+ 3)] [2,5]
    print $ mapapw (writer ((+ 3), "a")) (writer (5, "sd"))
    print $ mapapst (state $ \t -> (map toUpper, t + 1)) (state $ \x -> ("asd", x + 1)) 10
    printe $ mapape (Right (+ 5)) (Right 3)
    printe $ mapape (Right ('a' :)) (Right "asd")
    printe $ mapape (Right (+ 5)) (Left "err!")
    print $ map (\f -> f (Just 5)) [a1,ap1,ap1',am1,am3]
    printmapapr (*) (+ 5) [3,4,5,6,7]
