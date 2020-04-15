
import Data.Monoid

-- 12 Monoid

-- 12.1 The newtype keyword

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

test1 = CharList "this will be shown!"
test2 = CharList "benny" == CharList "benny"


-- 12.2 Using newtype to make type class instances

newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)

pair1 = getPair $ fmap (+100) (Pair (2, 3))
pair2 = getPair $ fmap reverse (Pair ("london calling", 3))


-- On newtype laziness

data CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

helloundef = helloMe undefined -- throw exception

newtype  CoolBoolN = CoolBoolN { getCoolBoolN :: Bool }

helloMeN :: CoolBoolN -> String
helloMeN (CoolBoolN _) = "hello"

helloundefN = helloMeN undefined

-- 12.3

--list

testList = 
    ([1,2,3] `mappend` [4,5,6],
    "one" `mappend` "two" `mappend` "three",
    "pang" `mappend` mempty,
    mempty :: [a])

-- Product and Sum

testProduct =
    map getProduct [Product 3 `mappend` Product 9,
                    Product 3 `mappend` mempty,
                    Product 3 `mappend` Product 4 `mappend` Product 2,
                    mconcat . map Product $ [3,4,2]]

testProduct2 = getProduct . mconcat . map Product

testSum =
    map getSum [Sum 3 `mappend` Sum 9,
                Sum 3 `mappend` mempty,
                Sum 1 `mappend` Sum 2 `mappend` Sum 3,
                mconcat . map Sum $ [1,2,3]]

testSum2 = getSum . mconcat . map Sum


-- Any and All

testAny =
    map getAny [Any True `mappend` Any False,
                mempty `mappend` Any True,
                mempty `mappend` mempty,
                mconcat . map Any $ [False,False,False,True]]

testAny2 = getAny . mconcat . map Any


testAll =
    map getAll [All True `mappend` All False,
               mempty `mappend` All True,
               mempty `mappend` mempty,
               mconcat . map All $ [True,True,True]]

testAll2 = getAll. mconcat . map All

-- Ordering monoid

testOrdering =
    (LT `mappend` GT,
    GT `mappend` LT,
    mempty `mappend` LT,
    mempty `mappend` GT)

lengthCompare x y = 
    (length x `compare` length y) `mappend` (x `compare` y)

test7 =
    (lengthCompare "zen" "ants",
    lengthCompare "zen" "ant")
 
testMaybe =
    (Nothing `mappend` Just "andy",
    Just LT `mappend` Nothing,
    Just (Sum 3) `mappend` Just (Sum 4))


testFirst =
    map getFirst [First (Just 'a') `mappend` First (Just 'b')
                 ,First Nothing `mappend` First (Just 'b')
                 ,First (Just 'a') `mappend` First Nothing
                 ,mconcat . map First $ [Nothing, Just 'a', Just 'b']]

testFirst2 = getFirst . mconcat . map First


foldl' f acc l =
    case l of
      [] -> acc
      (x:xs) -> foldl' f (f acc x) xs

foldr' f acc l =
    case l of
      [] -> acc
      (x:xs) -> f x (foldr' f acc xs)

main = do
    print testProduct
    print $ testProduct2 [2,4,5]
    print testSum
    print $ testSum2 [2,4,5]
    print testAny
    print testAll
    print $ testAll2 [True,True,False,True]
    print testFirst
    print $ testFirst2 [Nothing, Just 9, Just 10]
