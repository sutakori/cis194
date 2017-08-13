
--e1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--e2
fibs2 :: [Integer]
fibs2 = map (\t@(x,y) -> x+y) (zip (0:fibs2) (0:1:fibs2))

--e3
data Stream a = Cons a (Stream a)


instance Show a => Show (Stream a) where
  show a = show (take 20 (streamToList a))

streamToList :: Stream a -> [a]
streamToList s@(Cons h t) = h : (streamToList t)


--e4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f a@(Cons h t) = Cons (f h) (streamMap f t)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed seed a = Cons a (streamFromSeed seed (seed a))

--e5
nats :: Stream Integer
nats = streamFromSeed (+1) 0
