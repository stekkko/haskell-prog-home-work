module HW1 where

--Validating Credit Card Numbers
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = if x < 1 
                then [] 
                else let (d,m) = divMod x 10 in m : toDigitsRev d

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond [] = []
doubleEverySecond [x] = [x]
doubleEverySecond (x:y:xs) = x : (y * 2) : doubleEverySecond xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEverySecond . reverse

sumDigits :: [Integer] -> Integer
sumDigits = foldr (\ x -> (+) (sum $ toDigits x)) 0

validate :: Integer -> Bool
validate x = mod (sumDigits $ doubleEveryOther $ toDigits x) 10 == 0

--The Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)

--Working!!!
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 start end _ = [(start, end)]
hanoi n start end stock = hanoi (n - 1) start stock end ++ [(start, end)] ++ hanoi (n - 1) stock end start
