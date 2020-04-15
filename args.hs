
import System.Environment
import Data.List

-- 9.4

-- ./args first second w00t "multi word args"

main = do
    args <- getArgs
    name <- getProgName
    putStrLn ("ProgramName: " ++ name)
    mapM putStrLn args
