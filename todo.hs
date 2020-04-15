
import System.Environment

-- 9.5 Todo List

dispatch :: String -> [String] -> IO ()
dispatch cmd = case cmd of
  "add" -> add
  "view" -> view
  "remove" -> remove

main = do
   (cmd: rest) <- getArgs
   dispatch cmd rest

add :: [String] -> IO ()
add [fileName, items] = putStrLn ("add: " ++ fileName)

view (fileName:_) = putStrLn ("view: " ++ fileName)

remove [fileName, numberString] = putStrLn ("remove: " ++ fileName)
