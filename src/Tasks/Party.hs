{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tasks.Party where

import Tasks.Employee
import Data.Monoid
import Data.Tree
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ fun) (GL employees listFun) = GL (emp:employees) (listFun + fun)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL employees1 fun1) (GL employees2 fun2) = GL (employees1 ++ employees2) (fun1 + fun2)
    
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold func acc (Node root forest) = func root (foldr (flip (treeFold func)) acc forest)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gls = (glCons emp secondBest, firstBest) where
  firstBest  = mconcat . map fst $ gls
  secondBest = mconcat . map snd $ gls
  
maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry moreFun $ maxFunPair tree where
  maxFunPair (Node emp []) = (glCons emp mempty, mempty)
  maxFunPair (Node emp subtrees) = nextLevel emp $ map maxFunPair subtrees
  
main :: IO()
main = readFile "TestData/company.txt"
            >>= (\fContent -> mapM_ putStrLn (prepareResult $ read fContent))
                                   
prepareResult :: Tree Employee -> [String]
prepareResult company = ("Total fun: " ++ show fun) : map empName employees where
                            (GL employees fun) = maxFun company