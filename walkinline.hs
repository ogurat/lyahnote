
-- 13.4 Walk the line

type Birds = Int -- number of birds
type Pole = (Birds, Birds)


landLeft' :: Birds -> Pole -> Pole
landLeft' n (left, right) = (left + n, right)

landRight' :: Birds -> Pole -> Pole
landRight' n (left, right) = (left, right + n)


land' =
  let p1 = landLeft' 1 (0, 0)
      p2 = landRight' 1 p1
      p3 = landRight' (-1) p2
  in (p1, p2, p3)


land2 = landLeft' 2 (landRight' 1 (landLeft' 1 (0,0)))


x -: f = f x

testl = (0,0) -: landLeft' 2

land3 = (0, 0) -: landLeft' 1 -: landRight' 4 -: landLeft' (-1) -: landRight' (-2)


-- 失敗を返しうるように修正

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

test2 = landLeft 2  (0,0)
test3 = landLeft 10 (0,3)

test4 = landRight 1 (0,0) >>= landLeft 2
test5 = return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2

test6 = 
  return (0, 0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)

banana :: Pole -> Maybe Pole
banana _ = Nothing


test7 = return (0,0) >>= landLeft 1 >>= banana >>= landRight 1

testa = (Nothing >>        Just 3,
         Nothing >>= \_ -> Just 3,
	 Just 3 >> Just 4)

test8 = return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1


landdo n1 n2 n3 = do
    p0 <- return (0,0)
    p1 <- landLeft n1 p0
    p2 <- landRight n2 p1
    landLeft n3 p2


main = do
    print test2
    print test3
    print test4
    print test8
    print $ landdo 2 2 1
    print $ landdo 5 1 2

