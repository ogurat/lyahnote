

-- 10.2 Heathrow to London

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)

type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30,
                     Section 5 90 20,
                     Section 40 2 25,
                     Section 10 8 0
                   ]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pa, pb) (Section a b c) =
    let timeA = sum (map snd pa)
        timeB = sum (map snd pb)
        forwardTimeToA = timeA + a
        crossTimeToA = timeB + b + c
        forwardTimeToB = timeB + b
        crossTimeToB = timeA + a + c
        newA = if forwardTimeToA <= crossTimeToA
                then (A, a) : pa
                else (C,c): (B, b) : pb
        newB = if forwardTimeToB <= crossTimeToB
                then (B, b) : pb
                else (C,c): (A, a) : pa
    in (newA, newB)

optimalPath :: RoadSystem -> Path
optimalPath rs =
    let (bestA, bestB) = foldl roadStep ([], []) rs
    in if sum (map snd bestA) <= sum (map snd bestB)
        then reverse bestA
        else reverse bestB

test = optimalPath heathrowToLondon

groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        rs = map (\ [a,b,c] -> Section a b c) threes
        path = optimalPath rs
        pathString = concat $ map (show . fst) path
        pathTime = sum $ map snd path
    putStrLn $ "path: " ++ pathString ++ " time: " ++ show pathTime
