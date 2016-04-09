{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tasks.CalculatorTest where
import Tasks.ExprT
import Tasks.Parser
import Tasks.Calculator

import Test.Framework
import Test.HUnit (Assertion)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testExprT = testExp :: Maybe ExprT
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

test_eval :: Assertion
test_eval = assertEqual 20 $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

test_parseExp1 :: Assertion
test_parseExp1 = assertEqual (Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4)))
                             (parseExp Lit Add Mul "(2+3)*4")

test_evalStr1 :: Assertion
test_evalStr1 = assertEqual (Just 20) $ evalStr "(2+3)*4"

test_evalStr2 :: Assertion
test_evalStr2 = assertEqual (Just 120) $ evalStr "(17+3)*(4+2)"

test_Integer :: Assertion
test_Integer = assertEqual (Just (-7)) testInteger

test_ExprT :: Assertion
test_ExprT = assertEqual (Just (Add (Mul (Lit 3) (Lit (-4))) (Lit 5))) testExprT

test_Bool :: Assertion
test_Bool = assertEqual (Just True) testBool

test_MinMax :: Assertion
test_MinMax = assertEqual (Just (MinMax 5)) testMM

test_Sat :: Assertion
test_Sat = assertEqual (Just (Mod7 3)) testSat

test_MapExpr1 :: Assertion
test_MapExpr1 = assertEqual (Just 9) $ (withVars [("x", 6)] $ add (lit 3) (var "x"))

test_MapExpr2 :: Assertion
test_MapExpr2 = assertEqual Nothing $ (withVars [("x", 6)] $ add (lit 3) (var "y"))

test_MapExpr3 :: Assertion
test_MapExpr3 = assertEqual (Just 54) $ (withVars [("x", 6), ("y", 3)]
                                            $ mul (var "x") (add (var "y") (var "x")))