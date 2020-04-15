
-- 9.1

main = interact shortLinesOnly

{-
main = do
    cont <- getContents
    putStr (shortLinesOnly cont)
-}

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\l -> length l < 20) . lines
