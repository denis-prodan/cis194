{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Tasks.GolfTest where
import Tasks.Golf

import Test.Framework
import Test.HUnit (Assertion)

test_skips1 :: Assertion
test_skips1 = assertEqual ["ABCD", "BD", "C", "D"] $ skips "ABCD"

test_skips2 :: Assertion
test_skips2 = assertEqual ["hello!", "el!", "l!", "l", "o", "!"] $ skips "hello!"

test_skips3 :: Assertion
test_skips3 = assertEqual [[1]] $ skips [1]

test_skips4 :: Assertion
test_skips4 = assertEqual [[True,False], [False]] $ skips [True,False]

test_skips5 :: Assertion
test_skips5 = assertEqual [] $ skips ([]::[Int])

test_localMaxima1 :: Assertion
test_localMaxima1 = assertEqual [9,6] $ localMaxima  [2,9,5,6,1]

test_localMaxima2 :: Assertion
test_localMaxima2 = assertEqual [4] $ localMaxima [2,3,4,1,5]

test_localMaxima3 :: Assertion
test_localMaxima3 = assertEqual [] $ localMaxima [1,2,3,4,5]

test_histogram1 :: Assertion
test_histogram1 = assertEqual " *        \n *        \n *   *    \n==========\n0123456789\n" 
                                $ histogram [1,1,1,5]

test_histogram2 :: Assertion
test_histogram2 = assertEqual "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n" 
                                $ histogram  [1,4,5,4,6,6,3,4,2,4,9]