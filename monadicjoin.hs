
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Stack


-- 14.5


-- join

join' mm = mm >>= \m -> m

joinm (Just x)  = (\m -> m) x
joinm Nothing   = Nothing
joinm1 (Just x) = x
joinm1 Nothing  = Nothing

mapjoinm :: Maybe (Maybe a) -> [Maybe a]
mapjoinm mm =
    map (\x -> x mm) [join,join',joinm,joinm1]


joinl xs  = concat (map (\m -> m) xs)
joinl1 xs = concat xs

mapjoinl mm =
    map (\x -> x mm) [join,join',joinl,joinl1]

joinr f  = \w -> (\m -> m) (f w) w
joinr1 f = \w -> (f w) w

mapjoinr mm x =
    map (\a -> a mm x) [join,join',joinr,joinr1]

joine (Right x)   = (\m -> m) x
joine (Left err)  = Left err
joine1 (Right x)  = x
joine1 (Left err) = Left err

mapjoine mm =
    map (\x -> x mm) [join,join',joine,joine1]

je = joine (Right (Right 9)) -- :: Num a => Either String a

joinw w  = let (x,v) = runWriter w
               q = (\m -> m) x
               (y,v') = runWriter q
            in writer (y, v `mappend` v')
joinw1 w = let (x,v) = runWriter w
               (y,v') = runWriter x
            in writer (y, v `mappend` v')

mapjoinw mm =
    let fn test = runWriter $ test mm 
    in map fn [join,join',joinw,joinw1]


joinst :: State s (State s a) -> State s a
joinst sh  = let h = runState sh
            in state $ \s ->
                let (a, newSt) = h s
                    g = runState ((\m -> m) a)
        in g newSt
joinst1 :: State s (State s a) -> State s a
joinst1 sh = let h = runState sh
            in state $ \s ->
                let (a, newSt) = h s
                    g = runState a
        in g newSt

mapjoinst mm s =
    let fn test = runState (test mm) s 
    in map fn [join,join',joinst,joinst1]



testJoin = do
    --print $ runWriter jw
    print $ mapjoinm (Just (Just 9))
    print $ mapjoinm (Just Nothing :: Maybe (Maybe Int))
    print $ mapjoinl [[1,2],[4,5,6]]
    print $ mapjoinr (\x -> \y -> x ++ y) "abc"
    print $ mapjoine (Right (Right 8) :: Either String (Either String Int))
    print $ mapjoine (Left "err!" :: Either String (Either String Int))
    print $ mapjoinst (state $ \s -> (pop, 1:2:s)) [4,5,6]
