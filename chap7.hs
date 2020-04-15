
import qualified Data.Map as Map

-- 7.2 Algebraic data types intro

data Point = Point Float Float deriving (Show)
data Shape =
    Circle Point Float |
    Rectangle Point Point deriving (Show)

area :: Shape -> Float
area (Circle _  r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) =
    (abs $ x2 - x1) * (abs $ y2 - y1)

shapes = [Circle (Point 10 20) 10, Rectangle (Point 0 0) (Point 100 100)]
test7_2_1 = map area shapes

test7_2_2 = map (Circle (Point 10 20)) [4,5,6,6]
test7_2_3 = map area test7_2_2


nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
           Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

test7_2_4 = map nudge shapes

-- 7.3 Record syntax

{-
data Person = Person { firstName :: String,
                      lastName :: String,
                      age :: Int,
                      height :: Float,
                      phoneNumber :: String,
                      flavor ::  String } deriving (Show)
-}

data Car = Car { company :: String,
                 model :: String,
                 year :: Int } deriving (Show)
  
car = Car { company="Ford", model="Mustang", year=1967 }


-- 7.4 Type parameters


-- 7.5 Derived instances

data Person = Person { firstName :: String,
                      lastName :: String,
                      age :: Int } deriving (Eq, Show, Read)
mikeD = Person { firstName = "Micheal", lastName = "Diamond", age = 43 }
adRock = Person { firstName = "Adam", lastName = "Horovitz", age = 41 }
mca = Person { firstName = "Adam", lastName = "Yauch", age = 44 }

ppeqs = [mca == adRock, mikeD == adRock, mikeD == mikeD ]
ppeq = mikeD == Person { firstName = "Micheal", lastName = "Diamond", age = 43 }

beatieBoys = [mca,adRock, mikeD]
test7_5_1 = mikeD `elem` beatieBoys


data Day = Monday | Tuesday | Wednesday | Thursday | Friday |
           Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)

test7_5_2 =
    (Saturday == Sunday, Saturday > Friday,
     minBound :: Day, maxBound :: Day,
     succ Monday, succ Saturday, pred Saturday)

-- 7.6 Type synonyms

type AssocList k v =  [(k, v)]

--type IntMap = Map Int


data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup number map =
    case Map.lookup number map of
      Nothing -> Left $ "Locker " ++ show number ++ " doesn't exist!"
      Just (Free, code) -> Right code
      Just (Taken, _) -> Left $ "Locker " ++ show number ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]

lockertest =
    map (\n -> lockerLookup n lockers) [101,100,102,110,105]

-- 7.7 Recursive data structures

-- tree

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = Node x EmptyTree EmptyTree
treeInsert x (Node a l r)  -- x がOrdに制約されるこをコンパイラが推論する
    | x == a = Node x l r
    | x < a  = Node a (treeInsert x l) r
    | x > a  = Node a l (treeInsert x r)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a l r)
    | x == a = True
    | x < a  = treeElem x l
    | x > a  = treeElem x r

atree = foldr treeInsert EmptyTree [8,6,4,1,7,3,5]

atreeelem = map (`treeElem` atree) [8, 100,1,10,5,7]

-- 7.8 Typeclass

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red Light"
    show Yellow = "Yellow Light"
    show Green = "Green Light"

test7_8_1 =
    [Red == Red,Green == Green,Red == Green,Red /= Red,Red /= Green]
test7_8_1_1 =
    Red `elem` [Red, Yellow, Green]
test7_8_2 =
    [Red, Yellow, Green]


data Maybe' a = Nothing' | Just' a  deriving (Show)


instance (Eq m) => Eq (Maybe' m) where
    Just' x == Just' y = x == y
    Nothing' == Nothing' = True
    _ == _ = False

test7_8_3 =
    [Just' 1 == Just' 1, Just' 1 == Just' 2,Just' 1 /= Just' 1,Just' 1 /= Just' 2, Just' 1 == Nothing']

-- 7.9 Yes No

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

instance YesNo Bool where
    yesno False = False
    yesno _     = True

instance YesNo (Maybe' a) where
    yesno Nothing'  = False
    yesno (Just' _) = True

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

test7_9_1 =
    [yesno (length []), yesno "haha", yesno "", yesno (Just' 0), yesno Nothing', yesno True, yesno EmptyTree, yesno [], yesno Green]

yesnoIf val yesResult noResult =
    if yesno val
       then yesResult  else noResult

test7_9_2 =
    [yesnoIf [] "yeah!" "no!", yesnoIf [2,3,4] "yeah!" "no!", 
     yesnoIf (Just' 500) "yeah!" "no!", yesnoIf Nothing' "yeah!" "no!",
     yesnoIf True "yeah!" "no!"]


-- 7.10 Functor

{- Defined in ‘GHC.Base
instance Functor Maybe' where 
    fmap f (Just' x) = Just' (f x)
    fmap f Nothing' = Nothing'
-}

test7_10_1 =
    let a1 = fmap (++ " hey guys im inside the just") (Just "Something serious.")
        a2 = fmap (++ " hey guys im inside the just") Nothing
        a3 = fmap (+ 1) (Just 10)
        a4 = fmap (+ 1) Nothing
　　in (a1,a2,a3,a4)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

test7_10_2 =
    let a5 = fmap (* 5) atree
        a6 = fmap (* 4) (foldr treeInsert EmptyTree [7,3,5])
    in (a5, a6)

{--  ghc 7.6.3 defined in Data.Either
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
-}

test7_10_3 =
    let a5 = fmap (* 5) (Right 8)
        a6 = fmap (* 5) (Left 10)
    in (a5, a6)

