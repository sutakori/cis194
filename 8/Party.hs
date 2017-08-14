module Party where

import Employee
import Data.Monoid
import Data.Tree
import System.IO
import Data.List
import Data.String

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
treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold e f (Node x []) = f x []
treeFold e f (Node x chs) = f x (map (treeFold e f) chs)

--e3
nextLevel :: Employee
          -> [(GuestList, GuestList)] --(with current boss, without)
          -> (GuestList, GuestList)
nextLevel e glps = (glCons e $ foldr mappend mempty $ map snd glps,
                    foldr mappend mempty $ map (uncurry max) glps)

--e4
maxFun :: Tree Employee -> GuestList
maxFun t = (uncurry max) $ treeFold (GL [] 0, GL [] 0) nextLevel t

--e5
printAll :: Tree Employee -> IO()
printAll t = runall $ map putStrLn ss
  where gl@(GL es f) = maxFun t
        ss = ("Total fun:" ++ (show f)) : (map empName $ sortBy (\(Emp name1 _) (Emp name2 _)-> compare name1 name2) es)

runall :: [IO()] -> IO()
runall [] = return ()
runall (e:es) = do
  e
  runall es

main :: IO ()
main = do
  inpStr <- readFile "company.txt"
  inpTree <- readIO inpStr
  printAll inpTree
