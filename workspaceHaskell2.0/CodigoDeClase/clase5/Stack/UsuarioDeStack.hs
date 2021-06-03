import Stack1


-- emptyS,
-- isEmptyS,
-- push,
-- top,
-- pop,
-- lenS

apilar :: [a] -> Stack a
apilar xs = apilar' (reverse xs)

apilar' :: [a] -> Stack a
apilar' (x:[])   = push x (emptyS)
apilar' (x:xs)   = push x (apilar' xs)


desapilar :: Stack a -> [a]
desapilar s =  reverse (desapilar' s)


desapilar' :: Stack a -> [a]
desapilar' s = (if (isEmptyS s) then [] else (top s) : (desapilar' (pop s)))


