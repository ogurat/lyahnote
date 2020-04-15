
--import Control.Monad.Instances
--import Control.Monad.Error

-- 14.4 Error error on the wall

{-
instance Error e => Monad (Either a) where
    return x = Right x
    Right x >>= f = f x
    Left err >>= f = Left err
    fail msg = Left (strMsg msg)
-}

m1 = Left "boom" >>= \x -> return (x + 1)

m2 = Left "boom " >>= \x -> Left "no way!" :: Either String String

m3 = Right 100 >>= \x -> Left "no way!" :: Either String String

m4 = Right 3 >>= \x -> return (x + 100) :: Either String Integer

m5 = (Right "abc" >>=)

m6 = m5 (\x -> Right ('q' : x)) :: Either String String
m7 = m5 (\x -> Left "no way!") :: Either String String

main = do
    print m1
    print m2
    print m3
    print m4
    print m6
    print m7

