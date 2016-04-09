module Tasks.Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n -2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0:1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show x = show (take 20 $ streamToList x)

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

nats :: Stream Integer
nats = streamFromSeed (1+) 0

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) y = Cons x (streamInterleave y xs)

ruler :: Stream Integer
ruler = let integers = nats
            streams = streamMap streamRepeat integers
        in infiniteInterleave streams
        
infiniteInterleave :: Stream (Stream a) -> Stream a
infiniteInterleave (Cons x xs) = x `streamInterleave` (infiniteInterleave xs)