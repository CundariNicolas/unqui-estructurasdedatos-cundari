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

-- Costo: constante
emptyS :: Stack a
emptyS = (ConsS [])

-- Costo: constante
isEmptyS :: Stack a -> Bool
isEmptyS (ConsS xs) = null xs

-- Costo: constante
push :: a -> Stack a -> Stack a
push x (ConsS xs) = (ConsS (x:xs))


-- Costo: constante
top :: Stack a -> a
top (ConsS xs) = head xs

-- Costo: constante
pop :: Stack a -> Stack a
pop (ConsS s) = ConsS (tail s)

-- Costo: lineal
lenS :: Stack a -> Int
lenS (ConsS [])     = 0
lenS (ConsS (x:xs)) = 1 + lenS (ConsS xs)