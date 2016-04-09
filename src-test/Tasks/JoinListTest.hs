{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tasks.JoinListTest where

import Data.Monoid
import Test.Framework
import Test.HUnit (Assertion)

import Tasks.JoinList

yeah = Append (Product 210)
        (Append (Product 30)
            (Single (Product 5) 'y')
            (Append (Product 6)
                (Single (Product 2) 'e')
                (Single (Product 3) 'a')))
        (Single (Product 7) 'h')

test_concat :: Assertion
test_concat = assertEqual (Append (Product 6)
                                (Single (Product 2) 'e')
                                (Single (Product 3) 'a'))
                         $ (Single (Product 2) 'e') +++ (Single (Product 3) 'a')