
--import Control.Monad

import Control.Applicative

-- 13 A Fistful of Monads

-- 13.1 Applicative functors

test1 = (*) <$> Just 2 <*> Just 8
test2 = (++) <$> Just "exdeath" <*> Nothing
test3 = (-) <$> [3,4] <*> [1,2,3]


-- 13.2 Getting our feet wet with Maybe

test4 = fmap (++ "!") (Just "wisdom")
test5 = fmap (++ "!") Nothing

test6 = Just (+3) <*> Just 3
test7 = Nothing <*> Just "greed"
-- test8 = Just ord <*> Nothing

test9 = max <$> Just 3 <*> Just 6
test10 = max <$> Just 3 <*> Nothing


applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x

testAppMaybe = 
    (Just 3       `applyMaybe` \x -> Just (x + 1),
     Just "smile" `applyMaybe` \x -> Just(x ++ " :)"),
     Nothing      `applyMaybe` \x -> Just(x + 1),
     Nothing      `applyMaybe` \x -> Just(x ++ " :)"))

testApp2 =
    let app x = x `applyMaybe` \x -> if x > 2 then Just x else Nothing
    in (app (Just 3), app (Just 1))


-- 13.3 The Monad type class
{-
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing
-}

testreturn = return "WHAT" :: Maybe String
testjust = Just 9 >>= \x -> return (x*10)
testnothing = Nothing >>= \x -> return (x*10)



-- 13.5 do notation

testnest = Just 3 >>= (\x -> Just "!"  >>= (\y -> Just (show x ++ y)))

nothin = Nothing :: Maybe Int
testnest1 = nothin >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

testnest2 = Just 3 >>= (\x -> Nothing  >>= (\y -> Just (show x ++ y)))

testnest3 = Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing))

testnestdo :: Show a => Maybe a -> Maybe [Char] -> Maybe [Char]
testnestdo a b = do
    x <- a
    y <- b
    Just (show x ++ y)

marySue = Just 9 >>= (\x -> Just (x > 8))
marySue2 = do
    x <- Just 9
    Just (x > 8)

a1 = testnestdo (Just 3) (Just "!")

a2 = testnestdo (Just 3) Nothing

justH h = do
    (x:_) <- Just h
    return x

testJust = (justH "hello", justH "", justH [1,2,3])

main = do
    print testAppMaybe
    print testreturn
    print testJust
    print testnothing
    print testnest
    print testnest2
    print a1
    print a2
