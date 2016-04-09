{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} Tasks.LogAnalysisTest
import {-@ HTF_TESTS @-} Tasks.CreditCardValidationTest
import {-@ HTF_TESTS @-} Tasks.HanoiTest
import {-@ HTF_TESTS @-} Tasks.GolfTest
import {-@ HTF_TESTS @-} Tasks.FoldsTest
import {-@ HTF_TESTS @-} Tasks.CalculatorTest
import {-@ HTF_TESTS @-} Tasks.FibonacciTest
import {-@ HTF_TESTS @-} Tasks.JoinListTest

main :: IO()
main = htfMain htf_importedTests
