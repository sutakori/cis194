module Party where

import Employee
import Data.Monoid
import Data.Tree

--e1
glCons :: Employee -> GuestList -> GuestList
glCons e@Emp{empFun = fe} (GL es f) = GL (e:es) (f+fe)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 f1) (GL es2 f2) = GL (es1++es2) (f1+f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gs1@(GL _ f1) gs2@(GL _ f2) | f1<f2 =gs2
                              | otherwise = gs1

--e2
treeFold :: b -> ([b] -> a -> b) -> Tree a -> b
treeFold e f (Node x []) = f [] x
treeFold e f (Node x chs) = f (map (treeFold e f) chs) x
