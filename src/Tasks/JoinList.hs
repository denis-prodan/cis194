{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Tasks.JoinList where

import Tasks.Sized
import Tasks.Scrabble
import Tasks.Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
left +++ right = Append (tag left `mappend` tag right) left right

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

tagSize :: (Sized m, Monoid m) => JoinList m a -> Int
tagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                              = Nothing
indexJ index _ | index < 0                  = Nothing
indexJ index jList | index >= tagSize jList = Nothing
indexJ _ (Single _ a)                       = Just a
indexJ i (Append _ l r)
            | i < tagSize l = indexJ i l
            | otherwise = indexJ (i - tagSize l) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty                        = Empty
dropJ index jList
           | index <= 0              = jList
           | index >= tagSize jList  = Empty
dropJ _ single@(Single _ _)          = single
dropJ index (Append _ lList rList)
          | index < tagSize lList    = dropJ index lList +++ rList
          | otherwise                = dropJ (index - tagSize lList) rList

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty                       = Empty
takeJ index jList 
           | index <= 0             = Empty
           | index >= tagSize jList = jList
takeJ _ single@(Single _ _)         = single
takeJ index (Append _ lList rList)
          | index < tagSize lList   = takeJ index lList
          | otherwise               = lList +++ takeJ (index - tagSize lList) rList          
         
scoreLine :: String -> JoinList Score String
scoreLine string = Single (scoreString string) string

scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine string = Single (scoreString string, Size 1) string

tagScore :: JoinList (Score, Size) String -> Int
tagScore = getScore . fst . tag

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList

  fromString = fromLines . lines where
        fromLines [] = Empty
        fromLines [l] = scoreSizeLine l
        fromLines ls = fromLines (take half ls) +++
                       fromLines (drop half ls) where
                         half = length ls `div` 2
  line = indexJ

  replaceLine _ _ Empty = Empty 
  replaceLine index _ buf | index < 0 || index >= tagSize buf = buf
  replaceLine index ln buf =
    takeJ index buf +++ scoreSizeLine ln +++ dropJ (index + 1) buf

  numLines = tagSize
  value    = tagScore