module Queue1(
Queue,
emptyQ,
isEmptyQ,
queue,
firstQ,
dequeue
)
where


data Queue a = ConsQ [a] deriving Show

-- Implenentación:  Los elementos deben encolarse por el final de la lista y desencolarse por delante.

--Costo : constante
emptyQ :: Queue a
emptyQ = ConsQ []


--Costo : constante
isEmptyQ :: Queue a -> Bool
isEmptyQ (ConsQ x) = null x


--Costo : lineal
queue :: a -> Queue a -> Queue a
queue e (ConsQ x) = ConsQ (x ++ [e])



--Costo : constante
-- Debe tener al menos un elemento en la queue
firstQ :: Queue a -> a
firstQ (ConsQ qs) = head qs


--Costo : constante

dequeue :: Queue a -> Queue a
dequeue (ConsQ []) = (ConsQ [])
dequeue (ConsQ qs) = ConsQ (tail qs)





