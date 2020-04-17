import qualified Data.Map as Map
import Data.Char


phoneBook = Map.fromList $ phoneBook'

v = Map.lookup "grace" phoneBook 
newbook = Map.insert "grace" "341-9021" phoneBook

string2digits :: String -> [Int]
string2digits =  map digitToInt .filter isDigit

intBook = Map.map string2digits phoneBook
v2 = Map.lookup "betty" intBook


--phoneBookToMap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs

phoneBookToMap :: Ord k => [(k, a)] -> Map.Map k [a]
phoneBookToMap = Map.fromListWith (++) . map (\(k, v) -> (k, [v])) 

phoneBook' =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]

v3 = let pb = phoneBookToMap phoneBook'
        in map (\x -> Map.lookup x pb) ["patsy", "wendy", "john", "lucille"]
v4 = Map.lookup "patsy" $ phoneBookToMap phoneBook'
