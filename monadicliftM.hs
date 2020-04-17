
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Data.Char
import Stack


-- 14.5


-- liftM

liftM' :: Monad m => (t -> b) -> m t -> m b
liftM' f m = m >>= (\x -> return (f x))
liftM'' f m = do
    x <- m
    return (f x)

liftMaybe f (Just y) = (\x -> return (f x)) y
liftMaybe _ Nothing  = Nothing
liftMaybe' f (Just y) = Just (f y)
liftMaybe' _ Nothing  = Nothing

mapliftm mf m =
    map (\x -> x mf m) [fmap,liftM,liftM',liftMaybe,liftMaybe']

liftl  f xs = concat (map (\x -> return (f x)) xs)
liftl' f xs = concat (map (\x -> [f x]) xs)

mapliftl f xs  =
    map (\a -> a f xs) [fmap,liftM,liftM',liftl,liftl']



liftw f w =  let (x,v) = runWriter w
                 (y,v') = runWriter ((\m -> return (f m)) x)
            in writer (y,v `mappend` v')
liftw1 f w = let (x,v) = runWriter w
                 (y,v') = runWriter (return (f x))
            in writer (y,v `mappend` v')
liftw2 f w = let (x,v) = runWriter w
                 (y,v') = runWriter (writer ((f x), mempty))
            in writer (y,v `mappend` v')
liftw3 f w = let (x,v) = runWriter w
            in writer ((f x),v `mappend` mempty)


liftw' f w = let (x,v) = runWriter w
                 (y,v') = runWriter ((\m -> writer ((f m), mempty)) x)
            in writer (y,v `mappend` v')

mapliftw f x =
    let fn test = runWriter (test f x)
    in map fn [fmap,liftM,liftM',liftw,liftw',liftw1,liftw2,liftw3]


liftr  f h = \w -> (\x -> return (f x)) (h w) w
liftr' f h = \w -> (\x -> (\_ -> f x)) (h w) w
liftr2 f h = \w ->        (\_ -> f (h w)) w
liftr3 f h = \w ->               f (h w)

r1 = liftr2 (* 2) (+ 5)  -- \x -> (x + 5) * 2
r2 = liftr2 (*) (+ 5)    -- \x y -> (x + 5) * y
r3 = liftr (++ "!") (map toLower)

mapliftr :: (a -> b) -> (t -> a) -> t -> [b]
mapliftr f h x =
    map (\a -> a f h x) [fmap,liftM,liftM',liftr,liftr',liftr2,liftr3]


liftst f sth = let h = runState sth
    in state $ \s -> let (a,s') = h s
                         g = runState $ (\x -> return (f x)) a
                    in g s'
liftst1 f sth = let h = runState sth
    in state $ \s -> let (a,s') = h s
                         g = runState $ return (f a)
                    in g s'
liftst2 f sth = let h = runState sth
    in state $ \s -> let (a,s') = h s
                         g = runState $ state (\s -> ((f a), s))
                    in g s'
liftst3 f sth = let h = runState sth
    in state $ \s -> let (a,s') = h s
                         g = (\s -> ((f a), s))
                    in g s'
liftst4 f sth = let h = runState sth
    in state $ \s -> let (a,s') = h s
                    in ((f a), s')

stst =  runState (liftst (+ 5) pop)
ststm = runState (liftM' (+ 5) pop)

mapliftst f h x =
    let fn test = runState (test f h) x
    in map fn [fmap,liftM,liftM',liftst,liftst1,liftst2,liftst3,liftst4]

lifte f (Right y) = (\x -> return (f x)) y
lifte _ (Left err) = Left err

lifte' f (Right y) = return (f y)
lifte' _ (Left err) = Left err
lifte2 f (Right y) = Right (f y)
lifte2 _ (Left err) = Left err


maplifte f m =
    map (\x -> x f m) [fmap,liftM,liftM',lifte,lifte',lifte2]

{-
printmaplifte f m =
    mapM_ fn (maplifte f m)
    where fn :: Show a => Either String a -> IO ()
          fn = print
-}

printe :: Show a => [Either String a] -> IO ()
printe = print

main = do
    print $ mapliftm (^ 2) (Just 5)
    print $ mapliftm (^ 2) Nothing
    print $ mapliftl (^ 2) [3,4,5,6,7]
    print $ mapliftw (^ 2) (writer (3, "x log"))
    print $ mapliftr (* 2) (+ 5) 2
    print $ mapliftst (+ 2) pop [5,6,7,8]
    printe $ maplifte (+ 2) (Right 5)
    printe $ maplifte (\x -> x * x) (Right 3)
    printe $ maplifte (++ "!") (Right "asd")
    printe $ maplifte (\x -> x * x) (Left "err!")
    print $ r1 4
    print $ r2 4 5
    print $ r3 "QWERTY"
