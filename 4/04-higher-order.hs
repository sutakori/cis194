--e1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (\x -> x - 2) . filter even

--fun2' :: Integer -> Integer
--fun2' =

--e2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf
  where insertTree a Leaf = Node 0 Leaf a Leaf
        insertTree a (Node _ Leaf ta Leaf) = Node 1 (Node 0 Leaf a Leaf) ta Leaf
        insertTree a (Node _ l ta Leaf) = Node 1 l ta (Node 0 Leaf a Leaf)
        insertTree a (Node h l ta r@(Node h2 _ _ _)) | h == h2+2 = Node h l ta (insertTree a r)
        insertTree a (Node h l ta r) |  h==h'+1 = Node h insertL ta r
                                     |  h == h' = Node (h+1) insertL ta r
          where insertL@(Node h' _ _ _) = insertTree a l

--e3
xor :: [Bool] -> Bool
xor = foldr op False
  where op True = not
        op False = id

map' :: (a -> b) -> [a] -> [b]
--map' func = foldr (\x y -> (func x) : y) []
map' func = foldr ((:) . func) []


--e4
sieveSundaram :: Integer -> [Integer]
sieveSundaram = 
