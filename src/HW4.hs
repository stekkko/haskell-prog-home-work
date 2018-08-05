module HW4 where 

--exercise 1
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 1) . iterate checkN
  where 
    checkN n = if even n then n `div` 2 else n * 3 + 1
        

--exercise 2
data Tree a = Leaf
          | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

insert :: a -> Tree a -> Tree a
insert el Leaf = Node 0 Leaf el Leaf
insert el (Node d Leaf val r) = Node (max d 1) (insert el Leaf) val r
insert el (Node d l val Leaf) = Node (max d 1) l val (insert el Leaf)
insert el (Node _ l@(Node ld _ _ _) val r@(Node rd _ _ _))
  | ld <= rd   = let x@(Node d1 _ _ _) = insert el l 
                in Node (d1 + 1) x val r
  | otherwise = let x@(Node d1 _ _ _) = insert el r
                in Node (d1 + 1) l val x
                      

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

--exercise 3
xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

--exercise 4

seiveEratosphen :: [Int]
seiveEratosphen = filterPrime [2..]
  where
    filterPrime (p:xs) = p : filterPrime [x | x <- xs, x `mod` p /= 0]

seiveSundaram :: Int -> [Int]
seiveSundaram n = (map ((+1) . (*2)) . filter (`notElem` badList n)) [1..n]

badList :: Int -> [Int]
badList n = filter (<= n) $ map (\(x,y) -> x + y + 2*x*y) (cartProd [1..n] [1..n])

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
