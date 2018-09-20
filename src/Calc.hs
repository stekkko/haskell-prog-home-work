{-# LANGUAGE FlexibleInstances #-}
module Calc where

import ExprT
import Parser
--import qualified Data.Map as M

--exersize 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Mul x y) = eval x * eval y
eval (Add x y) = eval x + eval y

--exersize 2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
            Just p  -> Just (eval p)
            Nothing -> Nothing

--exersize 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  mul = Mul
  add = Add

reify :: ExprT -> ExprT
reify = id

--exersize 4
newtype MinMax = MinMax Integer deriving(Eq, Show)
newtype Mod7 = Mod7 Integer deriving(Eq, Show)

instance Expr Integer where
  lit x = x
  mul x y = x * y
  add x y = x + y

instance Expr Bool where
  lit x = x > 0
  mul x y = x && y
  add x y = x || y

instance Expr MinMax where
  lit = MinMax
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  add (MinMax x) (MinMax y) = MinMax (max x y)

instance Expr Mod7 where
  lit = Mod7
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)

testExp :: Expr a => Maybe a
testExp     = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

--exersize 6
class HasVars a where
  var :: String -> a

data VarExprT = Var String Integer 
           | Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

--instace HasVars VarExprT where
  --var = Var
  
