
-- 9.2

import System.IO
import System.Random

baabaa =
    do
        do
            putStrLn "1"
            h <- openFile "baabaa.txt" ReadMode
            cont <- hGetContents h
            putStr cont
            hClose h
        do
            putStrLn "2"
            withFile "baabaa.txt" ReadMode ( \h -> do
                cont <- hGetContents h
                putStr cont)
        do
            putStrLn "3"
            cont <- readFile "baabaa.txt"
            putStr cont

-- 9.6


testRandom =
    let r1 = random (mkStdGen 100) :: (Int, StdGen)
        r2 = random (mkStdGen 949494) :: (Int, StdGen)
        r3 = random (mkStdGen 949488) :: (Bool, StdGen)
    in (r1, r2, r3)

-- コイントス
threeCoins :: Int -> (Bool, Bool, Bool)
threeCoins seed =
    let gen = mkStdGen seed
        (first, gen') = random gen
        (second, gen'') = random gen'
        (third, gen''') = random gen''
    in (first, second, third)

testCoin =
    (threeCoins 21, threeCoins 22, threeCoins 943, threeCoins 944)


randoms' gen = let (value, newgen) = random gen
               in value:randoms' newgen

xxxxx = randoms' (mkStdGen 21)

