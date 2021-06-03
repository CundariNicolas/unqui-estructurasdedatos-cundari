module Queue2Listas(
Queue,
emptyQ,
isEmptyQ,
queue,
firstQ,
dequeue
)
where


data Queue a = ConsQ [a] [a] deriving Show

-- Implenentación:  Los elementos deben encolarse por el final de la lista y desencolarse por delante.

--Costo : constante
emptyQ :: Queue a
emptyQ = ConsQ [] []


--Costo : constante
isEmptyQ :: Queue a -> Bool
isEmptyQ (ConsQ fs bs) = null fs


--Costo : lineal
queue :: a -> Queue a -> Queue a
queue e (ConsQ [] bs) = ConsQ (e:[]) (e:bs)
queue e (ConsQ fs bs) = ConsQ fs (e:bs)



--Costo : constante
-- Debe tener al menos un elemento en la queue
firstQ :: Queue a -> a
firstQ (ConsQ fs bs) = head fs


--Costo : constante

dequeue :: Queue a -> Queue a
dequeue (ConsQ [] _)  = (ConsQ [] [])
dequeue (ConsQ fs bs) = ConsQ (last bs) (bs)



