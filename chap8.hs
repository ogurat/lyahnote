
import Control.Monad


-- 8.4

-- when
whenSmple = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input

-- sequence
seqSample = do
    do
        a1 <- getLine
        a2 <- getLine
        a3 <- getLine
        print [a1, a2, a3]
    do 
        rs <- sequence [getLine, getLine, getLine]
        print rs

listio =
    sequence (map print [1,2,3,4,5]) --IO [()]

listio2 = 
    mapM print [1,2,3,4,5] --IO [()]
    
listio3 =
    mapM_ print [1,2,3,4,5]

forMSample = do
    colors <- forM [1,2,3,4] $ \a -> do
        putStr ("which color " ++ show a ++ "? ")
        color <- getLine
        return color
    putStrLn "----> :"
    mapM putStrLn colors

