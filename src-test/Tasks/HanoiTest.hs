{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Tasks.HanoiTest where
import Tasks.Hanoi


import Test.Framework
import Test.HUnit (Assertion)

test_hanoi1 :: Assertion
test_hanoi1 = assertEqual [("a", "b")] 
                         $ hanoi 1 "a" "b" "c"

test_hanoi2 :: Assertion
test_hanoi2 = assertEqual [("a", "c"), ("a", "b"), ("c", "b")] 
                          $ hanoi 2 "a" "b" "c"
        
test_hanoi3 :: Assertion
test_hanoi3 = assertEqual [("a","b"),("a","c"),("b","c"),("a","b"),("c","a"),("c","b"),("a","b")] 
                          $ hanoi 3 "a" "b" "c"