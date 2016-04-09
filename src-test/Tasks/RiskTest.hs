{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Tasks.RiskTest where

import Tasks.Risk

import Test.Framework
import Test.HUnit (Assertion)

import Control.Monad
import Control.Monad.Random

test_successProb :: Assertion
test_successProb = (successProb (Battlefield 3 6)) >>= assertEqual (return 0.5)