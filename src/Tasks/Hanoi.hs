module Tasks.Hanoi (hanoi, Peg, Move) where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 peg1 peg2 _    = [(peg1, peg2)]
hanoi n peg1 peg2 peg3 = hanoi (n-1) peg1 peg3 peg2 
                        ++ [(peg1, peg2)] 
                        ++ hanoi (n-1) peg3 peg2 peg1