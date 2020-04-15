
import Data.Char

-- 9.1 Files and streams

main =
    getContents >>= putStr . map toUpper
{--
main = do
    contents <- getContents
    putStr (map toUpper contents)
--}
