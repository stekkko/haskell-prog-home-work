module Golf where

--exercize 1--
skip :: [a] -> Int -> [a]
skip [] _ = []
skip xs n = if (n > length xs) 
            then [] 
            else let sp = splitAt n xs 
                 in (last $ fst sp) : skip (snd sp) n

skips :: [a] -> [[a]]
skips []    = []
skips [x]   = [[x]]
skips list  = map (skip list) [1..(length list)]

--exercize 2--
localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:_)) = [y | y > x && y > z] ++ localMaxima xs
localMaxima _          = []

--exercize 3--
count :: [Integer] -> [Integer] -> [Integer]
count (x:xs) [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9] = case x of
                                         0 -> count xs [a0 + 1, a1, a2, a3, a4, a5, a6, a7, a8, a9]
                                         1 -> count xs [a0, a1 + 1, a2, a3, a4, a5, a6, a7, a8, a9]
                                         2 -> count xs [a0, a1, a2 + 1, a3, a4, a5, a6, a7, a8, a9]
                                         3 -> count xs [a0, a1, a2, a3 + 1, a4, a5, a6, a7, a8, a9]
                                         4 -> count xs [a0, a1, a2, a3, a4 + 1, a5, a6, a7, a8, a9]
                                         5 -> count xs [a0, a1, a2, a3, a4, a5 + 1, a6, a7, a8, a9]
                                         6 -> count xs [a0, a1, a2, a3, a4, a5, a6 + 1, a7, a8, a9]
                                         7 -> count xs [a0, a1, a2, a3, a4, a5, a6, a7 + 1, a8, a9]
                                         8 -> count xs [a0, a1, a2, a3, a4, a5, a6, a7, a8 + 1, a9]
                                         9 -> count xs [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9 + 1]
                                         _ -> count xs [a0, a1, a2, a3, a4, a5, a6, a7, a8, a9]
count _ list = list

putLn :: [Integer] -> Integer -> String
putLn [] _ = []
putLn (x:xs) m = (if x == m then '*' else ' ') : putLn xs m

decMaxByOne :: [Integer] -> Integer -> [Integer]
decMaxByOne [] _ = []
decMaxByOne (x:xs) m = (if x == m then x - 1 else x) : decMaxByOne xs m

construct :: [Integer] -> String
construct list = if maximum list == 0 
                 then "==========\n0123456789\n" 
                 else putLn list (maximum list) ++ "\n" ++ construct (decMaxByOne list (maximum list))

histogram :: [Integer] -> String
histogram list = construct (count list (take 10 [0,0..]))
