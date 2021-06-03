module Queue2(
Queue,
emptyQ,
isEmptyQ,
queue,
firstQ,
dequeue
)
where


data Queue a = ConsQ [a] deriving Show



--Costo : constante
emptyQ :: Queue a
emptyQ = ConsQ []


--Costo : constante
isEmptyQ :: Queue a -> Bool
isEmptyQ (ConsQ x) = null x


--Costo : constante
queue :: a -> Queue a -> Queue a
queue e (ConsQ x) = ConsQ (e:x)


--Costo : Lineal
firstQ :: Queue a -> a
firstQ (ConsQ qs) = last qs




--Costo : Lineal
dequeue :: Queue a -> Queue a
dequeue (ConsQ []) = (ConsQ [])
dequeue (ConsQ qs) = ConsQ (sinUltimoQ qs)



--Costo : Lineal
sinUltimoQ :: [a] -> [a]
sinUltimoQ (x:[]) = []
sinUltimoQ (x:xs) = x : sinUltimoQ xs



--Implemente ahora la versión que agrega por delante y quita por el final de la lista. Compare
--la eficiencia entre ambas implementaciones.