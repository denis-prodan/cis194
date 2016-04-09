module Tasks.Golf where

import Data.List

skips :: [a] -> [[a]]
skips xs = processIndexes (getNthElements xs) 1 (length xs)

-- Maps function from index to indexes from start to count
processIndexes :: (Int -> a) -> Int -> Int -> [a]
processIndexes func start cnt = map func . take cnt $ iterate (+ 1) start

-- skip (index - 1) element, take one item, concatenate with result of recursive call to rest of list
getNthElements :: [a] -> Int -> [a]
getNthElements [] _= []
getNthElements list ind = take 1 (drop (ind - 1) list) ++ getNthElements (drop ind list) ind

localMaxima :: [Integer] -> [Integer]
localMaxima xs = processIndexes (getLocalMax xs) 0 (length xs - 2) >>= id

getLocalMax :: Ord a => [a] -> Int -> [a]
getLocalMax arr startIndex = let localArr = take 3 (drop startIndex arr)
                             in if ((maximum localArr) == localArr !! 1)
                                then [localArr !! 1]
                                else []
                                
histogram :: [Int] -> String
histogram arr = let digitsAmounts = processIndexes (processDigits arr) 0 10
                in toStr $ processIndexes (toBools digitsAmounts) 1 $ maximum digitsAmounts

processDigits :: [Int] -> Int -> Int
processDigits arr digit = length $ elemIndices digit arr

toBools :: [Int] -> Int -> String
toBools arr cnt = map (\num -> if num < cnt then ' ' else '*') arr ++ "\n"

toStr :: [String] -> String
toStr histD = (reverse histD >>= id) ++ "==========\n0123456789\n"