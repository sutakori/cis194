module Golf where

skips :: [a] -> [[a]]
skips [] = []
skips xs =map (\y -> map takeFirst $ filter (\x@(_ , seq) -> seq `mod` y == 0) (zip xs [1..])) [1..length xs]
  where takeFirst (x , _) = x

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:[]) = []
localMaxima (_:_:[]) = []
localMaxima xs = map (\x@((_,s),_)->s) (filter (\x@((f,s),t)->f<s && s>t) (zip (zip xs (tail xs)) (tail $ tail xs)))

histogram :: [Integer] -> String
histogram ints = foldr ++ "==========\n0123456789" countStrings
  where countStrings counts = foldr ((:) . line) counts ++ countStrings (map next counts)
          where line x@(_,n) | n==0 = ' '
                             | otherwise = '*'
                next x@(num,n) | n==0 = (num,0)
                               | otherwise = (num,n-1)
                counts = count ints (zip [0..9] (repeat 0))
                  where count [] res = res
                        count (x:xs) res = map (\item@(num,n) -> if(x==num) then (num,n+1) else item) res
