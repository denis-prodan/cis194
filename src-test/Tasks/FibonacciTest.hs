{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Tasks.FibonacciTest where

import Test.Framework
import Test.HUnit (Assertion)

import Tasks.Fibonacci


test_fib :: Assertion
test_fib = assertEqual 8 $ fib 6

test_fibs1 :: Assertion
test_fibs1 = assertEqual [0, 1, 1, 2, 3, 5, 8] $ take 7 fibs1

test_fibs2 :: Assertion
test_fibs2 = assertEqual [0, 1, 1, 2, 3, 5, 8] $ take 7 fibs2

test_streamRepeat :: Assertion
test_streamRepeat = assertEqual (take 20 [1, 1 ..]) (take 20 $ streamToList $ streamRepeat 1)

test_streamMap :: Assertion
test_streamMap = assertEqual (take 20 [5, 5 ..]) (take 20 $ streamToList $ streamMap (+3) $ streamRepeat 2)

test_streamFromSeed :: Assertion
test_streamFromSeed = assertEqual (take 20 [1..]) (take 20 $ streamToList $ streamFromSeed (1+) 1)

test_nats :: Assertion
test_nats = assertEqual (take 20 [0..]) (take 20 $ streamToList nats)

test_streamInterleave :: Assertion
test_streamInterleave = assertEqual [0, 0, 0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0, 8, 0, 9] 
                                    (take 20 $ streamToList $ streamInterleave (streamRepeat 0) nats)
                                    
test_ruler :: Assertion
test_ruler = assertEqual [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4] 
                                    (take 16 $ streamToList ruler)