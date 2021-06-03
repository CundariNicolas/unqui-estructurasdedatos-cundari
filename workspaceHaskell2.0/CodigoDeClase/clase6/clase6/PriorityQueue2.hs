module PriorityQueue1(
  	PriorityQueue,
  	emptyPQ,
  	isEmptyPQ,
  	insertPQ,
  	findMinPQ,
  	deleteMinPQ
	) where


pq1= insertPQ 1 $
	 insertPQ 34 $
	 insertPQ 2 $
	 insertPQ 41 $
	 insertPQ 22 $
	 insertPQ 54 $
	 insertPQ 5 $
	 insertPQ 7 $
	 insertPQ 4 $
	 insertPQ 3 $
	 emptyPQ



data PriorityQueue a = PQ [a] deriving Show
-- Invariantes de representación:
-- 1. la lista está ordenada de menor a mayor

-- Costo: constante, O(1)
emptyPQ :: PriorityQueue a
emptyPQ = PQ []

-- Costo: constante, O(1)
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

-- Costo: lineal, O(n)
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ xs) = PQ (insertarPQ x xs)

insertarPQ :: Ord a => a -> [a] -> [a]
insertarPQ a [] =  (a:[])
insertarPQ a (x:xs) = if (a < x) then a:x:xs else x: (insertarPQ a xs) 
-- insertar de tal manera que la lista quede ordenada

-- Costo: constante, O(1)
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (PQ xs) = head xs

-- Costo: constante, O(1)
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (PQ xs) = PQ (tail xs)

-- borra el que sea el minimo
borrar m [] = []
borrar m (x:xs) =
  if m == x
     then xs
     else x : borrar m xs