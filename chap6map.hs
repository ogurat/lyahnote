import qualified Data.Map as Map
import Data.Char


phoneBook = Map.fromList $ phoneBook'

v = Map.lookup "grace" phoneBook 
newbook = Map.insert "grace" "341-9021" phoneBook

string2digits :: String -> [Int]
string2digits =  map digitToInt .filter isDigit

intBook = Map.map string2digits phoneBook
v2 = Map.lookup "betty" intBook

phoneBookToMap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs

phoneBook' = [("betty", "555-2938")
              ,("betty", "342-2492")
              ,("bonnie", "452-2928")
              ,("patsy", "439-2928")
              ,("patsy", "943-2928")
    ]


v3 = Map.lookup "patsy" $ phoneBookToMap phoneBook'