module Set2 (
	Set,
	emptyS,
	addS,
	belongs,
	sizeS,
	removeS,
	unionS,
	setToList
 ) where

data Set a = ConsS [a]
-- Invariantes de Representacion
-- No hay

-- Costo: constante
emptyS :: Set a
emptyS = ConsS []

-- Costo: constante
addS :: Eq a => a -> Set a -> Set a
addS x (ConsS xs) = ConsS (x:xs)

-- Costo: lineal
-- elem

-- Costo: lineal
belongs :: Eq a => a -> Set a -> Bool
belongs x (ConsS xs) = elem x xs

-- Costo: lineal
sizeS :: Eq a => Set a -> Int
sizeS (ConsS xs) = contarSinRepetidos xs

-- Costo: cuadrática
contarSinRepetidos :: Eq a => [a] -> Int
contarSinRepetidos [] 	  = 0
contarSinRepetidos (x:xs) = if (elem x xs) then contarSinRepetidos xs else 1 + contarSinRepetidos xs

-- Costo: lineal
removeS :: Eq a => a -> Set a -> Set a
removeS x (ConsS xs) = ConsS (sacarAparicionesDe x xs)

-- Costo: lineal
sacarAparicionesDe :: Eq a => a -> [a] -> [a]
sacarAparicionesDe x [] 	= []
sacarAparicionesDe x (y:ys) = if (x == y) then sacarAparicionesDe x ys else y : sacarAparicionesDe x ys

-- Costo: lineal
unionS :: Eq a => Set a -> Set a -> Set a
unionS (ConsS xs) (ConsS ys) = ConsS (append xs ys)

-- Costo: lineal
append [] ys = ys
append (x:xs) ys = x : append xs ys

-- Sin repetidos
-- Costo: cuadrática
setToList :: Eq a => Set a -> [a]
setToList (ConsS xs) = sinRepetidos xs

-- Costo: cuadrática
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if (elem x xs) then sinRepetidos xs else x: sinRepetidos xs