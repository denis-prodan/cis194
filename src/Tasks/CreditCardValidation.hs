module Tasks.CreditCardValidation (validate) where

validate :: Integer -> Bool
validate = checkRemainder . sumDigits . doubleEveryOther . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = x `mod` 10 : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [_] = []
doubleEveryOther (x:y:zs) = x:(y * 2): doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits = sum . flatMap toDigitsRev

checkRemainder :: Integer -> Bool
checkRemainder x =  (x `mod` 10) == 0

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap _ [] = []
flatMap f (x:xs) = f x ++ flatMap f xs