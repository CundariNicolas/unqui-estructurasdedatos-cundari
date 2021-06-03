module Stack1(
Stack,
emptyS,
isEmptyS,
push,
top,
pop,
lenS
)
where

data Stack a = ConsS [a] deriving Show


emptyS :: Stack a

isEmptyS :: Stack a -> Bool

push :: a -> Stack a -> Stack a


top :: Stack a -> a


pop :: Stack a -> Stack a


lenS :: Stack a -> Int