
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
