
-- 14.3 Tasteful stateful computations

-- Stacks and stones

import Control.Monad.State

{-
newtype State s a = State { runState :: s -> (a, s) }
instance Monad (State s) where
    return x = State (\s -> (x, s))
    (State h) >>= f = State $ \s -> let (a, newSt) = h s
                                        (State g) = f a
			                in g newSt
-}

type Stack = [Int]

pop :: State Stack Int
pop = state (\ (x:xs) -> (x, xs))

push :: Int -> State Stack ()
push a = state (\xs -> ((), a:xs))

stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    if a == 100
        then stackStuff
        else return ()

-- push 3自体が、スタックの状態をとり、3をpushした新しいスタックを作る関数（を中に入れている）
push3 = (push 3)


push3pop = (push 3) >>= \x -> pop
poppoppop = pop >>= \x -> pop >>= \y -> pop
poppush = pop >>= \x -> push (x * 2)
popp = pop >>= \x -> pop >>= \y -> pop >>= \z -> pop >>= \_ -> return (x+y+z)

runStack =
    let rs st = print (runState st [10,5,8,2,1])
    in do
        rs push3
        rs push3pop
        rs poppoppop
        rs poppush
        rs stackManip
        rs popp

runTest = do
    print (runState push3 [1,2,3])
    print (runState push3pop [1,2,3])
    print (runState poppoppop [1,2,3,4,5])
    print (runState poppush [5,8,2,1])
    print (runState stackManip [5,8,2,1])
    print (runState stackStuff [9,0,2,1,3])
    print (runState moreStack [9,0,2,1,3])

main = do
    runStack
