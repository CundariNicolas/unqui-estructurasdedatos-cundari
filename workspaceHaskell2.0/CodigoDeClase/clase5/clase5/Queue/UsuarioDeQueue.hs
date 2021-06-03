import Queue1





-- emptyQ,
-- isEmptyQ,
-- queue,
-- firstQ,
-- dequeue




--Costo1 : constante

--Costo2 : lineal
lengthQ :: Queue a -> Int
lengthQ q = if (isEmptyQ q) then 0 else 1 + lengthQ (dequeue q)



--Costo1 : constante
--Costo2 : cuadratica

queueToList :: Queue a -> [a]
queueToList q = if (isEmptyQ q) then [] else (firstQ q) : queueToList (dequeue q)



--Costo1 : lineal
--Costo2 : lineal
unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = listToQ (reverse (queueToList q1 ++ queueToList q2))


listToQ :: [a] -> Queue a
listToQ []     = emptyQ
listToQ (x:xs) = queue x (listToQ xs)




