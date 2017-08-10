{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified Data.Map as M

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Mul t1 t2) = eval t1 * eval t2
eval (Add t1 t2) = eval t1 + eval t2

evalStr :: String -> Maybe Integer
evalStr exp = case (parseExp Lit Add Mul exp) of
  Nothing -> Nothing
  (Just exp) -> Just (eval exp)

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  mul x y = Mul x y
  add x y = Add x y

instance Expr Integer where
  lit x = x
  mul x y = x * y
  add x y = x + y

instance Expr Bool where
  lit x | x <= 0 = False
        | otherwise = False
  mul x y = x && y
  add x y = x || y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  mul x@(MinMax nx) y@(MinMax ny) = MinMax (min nx ny)
  add x@(MinMax nx) y@(MinMax ny) = MinMax (max nx ny)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  mul x@(Mod7 nx) y@(Mod7 ny) = Mod7 (mod (nx*ny) 7)
  add x@(Mod7 nx) y@(Mod7 ny) = Mod7 (mod (nx+ny) 7)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

--e6
class HasVars a where
  var :: String -> a

data VarExprT = VarLit Integer
           | VarAdd VarExprT VarExprT
           | VarMul VarExprT VarExprT
           | VarVar String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit x = VarLit x
  add x y = VarAdd x y
  mul x y = VarMul x y

instance HasVars VarExprT where
  var x = VarVar x

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = \m -> M.lookup x m
