
import Control.Monad

-- 13.6 A knight's quest

--f <=< g = (\x -> g x >>= f)

type KnightPos = (Int, Int)


moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = do  
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]  
    guard (c' `elem` [1..8] && r' `elem` [1..8])  
    return (c',r')

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` (in3 start)

-- 14.7

compose n = foldr (<=<) return (replicate n moveKnight)

compose' 0 = return
compose' n = moveKnight <=< compose' (n - 1)
inMany x start = return start >>= compose' x

main = do
    print $ (6,2) `canReachIn3` (6,1)
    print $ inMany 3 (6,2)
    print $ in3 (6,2)
