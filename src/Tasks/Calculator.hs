{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances  #-}
module Tasks.Calculator where

import qualified Data.Map.Strict as M


import Tasks.ExprT
import Tasks.Parser

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class Expr a where
    lit :: Expr a => Integer -> a
    mul :: Expr a => a -> a -> a
    add :: Expr a => a -> a -> a
    
class HasVars a where
    var :: String -> a
    
withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup
    
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x _ = Just x
    add a b mp = case (a mp, b mp) of
                        (Just a', Just b') -> Just (a' + b')
                        _                  -> Nothing
    mul a b mp = case (a mp, b mp) of
                        (Just a', Just b') -> Just (a' * b')
                        _                  -> Nothing

    
data VarExprT = 
             VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | VVar String
  deriving (Show, Eq)
  
instance HasVars VarExprT where
    var = VVar
    
instance Expr VarExprT where
    lit = VLit
    mul = VMul
    add = VAdd

instance Expr ExprT where
    lit = Lit
    mul = Mul
    add = Add
    
instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

instance Expr Bool where
    lit = (>0)
    mul = (||)
    add = (&&)
    
instance Expr MinMax where
    lit                       = MinMax
    mul (MinMax x) (MinMax y) = MinMax $ min x y
    add (MinMax x) (MinMax y) = MinMax $ max x y

instance Expr Mod7 where
    lit x                 = Mod7 (abs x `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 (abs (x * y) `mod` 7)
    add (Mod7 x) (Mod7 y) = Mod7 (abs(x + y) `mod` 7)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Mul x y) = eval x * eval y
eval (Add x y) = eval x + eval y

evalStr :: String -> Maybe Integer
evalStr str = eval <$> parseExp Lit Add Mul str

reify :: ExprT -> ExprT
reify = id