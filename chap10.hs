
-- 10.1
--RPN Calculator

evalRPN :: String -> Double
evalRPN = head . foldl fn [] . words
    where fn (x:y:ys) "*" = (y * x):ys
          fn (x:y:ys) "/" = (y / x):ys
          fn (x:y:ys) "+" = (y + x):ys
          fn (x:y:ys) "-" = (y - x):ys
          fn (x:y:ys) "^" = (y ** x):ys
          fn (x:xs) "sin" = (sin x):xs
          fn (xs) "pi" = pi: xs
          fn xs x = read x:xs

testRPN =
    map evalRPN ["1 2 +"
                ,"10 4 3 + 2 * -"
                ,"2 3.5 +"
                ,"90 34 12 33 55 66 + * - +"
                ,"90 34 12 33 55 66 + * - + -"
                ,"90 3.8 -"
                ,"10 2 ^"
                ,"pi 2 / sin"]
