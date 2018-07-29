module Golf where

--exercize 1--
skip :: [a] -> Int -> [a]
skip [] _ = []
skip xs n = if n > length xs
            then [] 
            else let sp = splitAt n xs 
                 in last (fst sp) : skip (snd sp) n

skips :: [a] -> [[a]]
skips []    = []
skips [x]   = [[x]]
skips list  = map (skip list) [1..(length list)]

--exercize 2--
localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:_)) = [y | y > x && y > z] ++ localMaxima xs
localMaxima _          = []

--exercize 3--
count :: [Int] -> [Int] -> [Int]
count (x:xs) c = let sp = splitAt x c 
                 in count xs (fst sp ++ [1 + c !! x] ++ tail (snd sp))
count _ list = list

putLn :: [Int] -> Int -> String
putLn [] _ = []
putLn (x:xs) m = (if x == m then '*' else ' ') : putLn xs m

decMaxByOne :: [Int] -> Int -> [Int]
decMaxByOne [] _ = []
decMaxByOne (x:xs) m = (if x == m then x - 1 else x) : decMaxByOne xs m

construct :: [Int] -> String
construct list = if maximum list == 0 
                 then "==========\n0123456789\n" 
                 else putLn list (maximum list) ++ "\n" ++ construct (decMaxByOne list (maximum list))

histogram :: [Int] -> String
histogram list = construct (count list (take 10 [0,0..]))
