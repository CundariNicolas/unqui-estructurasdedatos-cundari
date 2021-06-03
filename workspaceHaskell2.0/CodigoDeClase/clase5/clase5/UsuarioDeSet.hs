import Set2
-- import Set2

-- Costo 1: cuadratica
-- Costo 2: ?
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (toSet xs)

-- Costo 1: cuadratica
-- Costo 2: ?
toSet :: Eq a => [a] -> Set a
toSet [] = emptyS
toSet (x:xs) = addS x (toSet xs)

-- Costo 1: cuadratica, porque belongs es cuadratica
-- Costo 2: ?
verSiEstan :: Eq a => [a] -> Set a -> [Bool]
verSiEstan [] s = []
verSiEstan (x:xs) s = belongs x s : verSiEstan xs s

-- Costo 1: cuadratica, porque belongs es cuadratica
-- Costo 2: ?
cuantosEstan :: Eq a => [a] -> Set a -> Int
cuantosEstan [] s = 0
cuantosEstan (x:xs) s = 
	if belongs x s
	   then 1 + cuantosEstan xs s
	   else cuantosEstan xs s

s1 :: Set Int
s1 = toSet [1,1,2,2,3,4,4,5,6,7,8,9,10,10,10,10]

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show


losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] 	set  = []
losQuePertenecen (x:xs) set  = if belongs x set then x : losQuePertenecen xs set else losQuePertenecen xs set

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos  EmptyT = emptyS
unirTodos (NodeT set i d) = unionS set $ unionS (unirTodos i) (unirTodos d)

