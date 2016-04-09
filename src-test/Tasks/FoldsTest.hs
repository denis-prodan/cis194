{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Tasks.FoldsTest where
import Tasks.Folds

import Test.Framework
import Test.HUnit (Assertion)

test_fun1 :: Assertion
test_fun1 = assertEqual 
                        (fun1 $ take 20 $ iterate (+ 1) 0)
                        (fun11 $ take 20 $ iterate (+ 1) 0)
                        
test_fun2 :: Assertion
test_fun2 = assertEqual  (map fun2 $ take 20 $ iterate (+ 1) 1) 
                         (map fun21 $ take 20 $ iterate (+ 1) 1)
                         
test_foldTree :: Assertion
test_foldTree = assertEqual
            (Node 4 
                (Node 2 
                    Leaf 
                    'G' 
                    (Node 1 
                        Leaf 
                        'F'
                        (Node 0 
                            Leaf 
                            'D'
                             Leaf)))
             'J'
                (Node 3 
                    (Node 1 
                        Leaf 
                        'C' 
                        (Node 0 
                            Leaf 
                            'B'
                             Leaf)) 
                    'I' 
                    (Node 2 
                        Leaf 
                        'H'
                        (Node 1 
                            Leaf 
                            'E' 
                            (Node 0 
                                Leaf 
                                'A'
                                Leaf)))))
            $ foldTree "ABCDEFGHIJ"

test_xor1 :: Assertion
test_xor1 = assertEqual True $ xor [False, True, False]

test_xor2 :: Assertion
test_xor2 = assertEqual False $ xor [False, True, False, False, True]

test_map' :: Assertion
test_map' = assertEqual (map(*2) [1..10])
                        (map' (*2) [1..10])

test_sieveSundaram1 :: Assertion
test_sieveSundaram1 = assertEqual [3, 5, 7, 11, 13, 17, 19] 
                                  $ sieveSundaram 10                       
