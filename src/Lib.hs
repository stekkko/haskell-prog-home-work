module Lib where

--Validating Credit Card Numbers
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = if x < 1 
                then [] 
                else (x `mod` 10) : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse $ toDigitsRev x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther l@(x:xs) = if odd $ length l 
                            then x : doubleEveryOther xs 
                            else (x * 2) : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = foldr1 (+) (if x == 0 then [0] else toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = mod (sumDigits $ doubleEveryOther $ toDigits x) 10 == 0

--The Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)

--not working
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi x a b c = (if odd x then (a, b) : (hanoi (x - 1) a b c) else (a, c) : (hanoi (x - 1) a c b))













  


