module Tasks.Folds where
import Data.List

data Tree a = Leaf
               | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)
    
    
foldTree :: [a] -> Tree a
foldTree = foldr insertInTree Leaf

insertInTree :: a -> Tree a -> Tree a
insertInTree x Leaf = Node 0 Leaf x Leaf
insertInTree x (Node n t1 val t2) 
    | h1 < h2   = Node  n   t1n val t2 
    | h1 > h2   = Node  n    t1 val t2n 
    | otherwise = Node (h+1) t1 val t2n  
  where h1  = heightTree t1
        h2  = heightTree t2
        t1n = insertInTree x t1
        t2n = insertInTree x t2
        h   = heightTree t2n

heightTree :: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node n _ _ _) = n

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs
    
fun11 :: [Integer] -> Integer
fun11 = product . map (\x -> x -2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)
       
fun21 :: Integer -> Integer
fun21 = sum . 
        filter even . 
        takeWhile (>1) . 
        iterate (\x -> if even x 
            then x `div` 2 
            else 3 * x + 1)
            
xor :: [Bool] -> Bool
xor = foldl (\x acc -> if x then not acc
                            else acc)
            False
            
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a acc-> (f a):acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs =  foldr (\b g x -> g (f x b)) id xs base 

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\y -> y*2 + 1) $ [1 .. n] \\
                                        [i + j + 2 * i * j | 
                                            i <- [1 .. (n `div` 2)], 
                                            j <- [1 .. (n `div` 2)], 
                                            i <= j && i + (j + 2 * i * j) <= n]