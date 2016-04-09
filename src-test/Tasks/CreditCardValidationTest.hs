{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Tasks.CreditCardValidationTest where
import Tasks.CreditCardValidation


import Test.Framework
import Test.HUnit (Assertion)

test_validateOk :: Assertion
test_validateOk = assertEqual True $ validate 4012888888881881

test_validateFail :: Assertion
test_validateFail = assertEqual False $ validate 4012888888881882