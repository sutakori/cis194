module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (mappend (tag jl1) $ tag jl2) jl1 jl2

--for check
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _ = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)


indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ i jl | i > (getSize $ size $ tag jl) || i <= 0 = Nothing
indexJ i (Single _ a) | i == 1 = Just a
indexJ i (Append m jl1 jl2) | i <= (getSize $ size $ tag jl1) = indexJ i jl1
                            | otherwise = indexJ i jl2

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ = 
