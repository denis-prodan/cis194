{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tasks.Risk where

import Data.List
import Control.Monad
import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
     deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield att def) = let 
                                 attackerRolls = fmap rolls getAttackingArmy att 
                                 defenderRolls = fmap rolls getDefendingArmy att 
                                 casualties = liftM2 getCasualties attackerRolls defenderRolls
                                 newAttackers = liftM2 (-) (return att) (liftM fst casualties)
                                 newDefenders = liftM2 (-) (return def) (liftM snd casualties)
                               in liftM2 Battlefield newAttackers newDefenders
                               
invade :: Battlefield -> Rand StdGen Battlefield
invade battlefield@(Battlefield att def) 
                                    | att < 2   = return battlefield
                                    | def == 0  = return battlefield
                                    | otherwise = battle battlefield >>= invade                                                     
                                                     
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do result <- replicateM 1000 (invade bf)
                    let     
                        defendersWin = filter (\bfRes -> defenders bfRes > 0) result
                        in return  $ (fromIntegral (1000 - length defendersWin)) / 1000

getAttackingArmy :: Army -> Int
getAttackingArmy army = min 3 (army - 1)

getDefendingArmy :: Army -> Int
getDefendingArmy army = min 2 army

rolls :: Int -> Rand StdGen [DieValue]
rolls battleArmySize = replicateM battleArmySize die

getCasualties :: [DieValue] -> [DieValue] -> (Int, Int)
getCasualties attackingRolls defendingRolls = foldr casualties (0, 0) $ zip 
                                                                    (sortBy (flip compare) attackingRolls) 
                                                                    (sortBy (flip compare) defendingRolls) where               
                                                casualties (attackRoll, defendRoll) (attackCas, defendCas) = let
                                                    attackersWin = unDV attackRoll > unDV defendRoll
                                                    in if attackersWin 
                                                            then (attackCas, defendCas + 1)
                                                            else (attackCas + 1, defendCas)
