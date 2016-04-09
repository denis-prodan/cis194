{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tasks.Scrabble where

import Data.Monoid()
import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score a = scoreChar $ toLower a where
  scoreChar c
    | c `elem` "aeilnorstu" = Score  1
    | c `elem` "dg"         = Score  2
    | c `elem` "bcmp"       = Score  3
    | c `elem` "fhvwy"      = Score  4
    | c `elem` "k"          = Score  5
    | c `elem` "jx"         = Score  8
    | c `elem` "qz"         = Score 10
    | otherwise             = Score  0

scoreString :: String -> Score
scoreString = mconcat . map score

getScore :: Score -> Int
getScore (Score i) = i