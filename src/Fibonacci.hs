{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Fibonacci where

--exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : kek 0 1 []
    where kek x y xs = (x + y) : kek y (x + y) xs

--exercise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a stream) = a : streamToList stream 

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

--exercise 4
streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a stream) = Stream (f a) (streamMap f stream)
 
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x)) 

--exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x s1) (Stream y s2) =
                  Stream x (Stream y (interleaveStreams s1 s2)) 

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..1000])

--exercise 6 (optional)

--exercise 7 (optional)
data Matrix = Matrix Integer Integer 
                     Integer Integer deriving(Show)
instance Num Matrix where
  (*) (Matrix a11 a12 
              a21 a22) 
      (Matrix b11 b12 
              b21 b22) = Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22)
                                (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22)
  (+) (Matrix a11 a12 
              a21 a22) 
      (Matrix b11 b12 
              b21 b22) = Matrix (a11 + b11) (a12 + b12)
                                (a21 + b21) (a22 + b22)
  negate (Matrix a11 a12 
                 a21 a22) = Matrix (-a11) (-a12)
                                   (-a21) (-a22)
  fromInteger x = Matrix x 0 0 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = let (Matrix _ a _ _) = (Matrix 1 1 1 0) ^ n in a
  
