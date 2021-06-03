import Map3

-- Interfaz
-- emptyM :: Map k v
-- assocM :: Eq k => k -> v -> Map k v -> Map k v Costo : Lineal
-- lookupM :: Eq k => k -> Map k v -> Maybe v 
-- deleteM :: Eq k => k -> Map k v -> Map k v
-- keys :: Map k v -> [k]  costo: COSTANTE


-- COSTO3 : Cuadrático porque cada elemento de ObtValores hace una lineal
valores :: Eq k => Map k v -> [v]
valores m = obtenerValores (keys m) m

obtenerValores :: Eq k => [k] -> Map k v -> [v]
obtenerValores [] m     = []
obtenerValores (k:ks) m = 
	valor (lookupM k m) : obtenerValores ks m

-- data Maybe a = Nothing | Just a

-- Parcial cuando es Nothing
valor :: Maybe v -> v
valor Nothing = error "no se obtener un valor"
valor (Just x) = x


-- COSTO3 : Lineal por lookup
claveExiste :: Eq k => k -> Map k v -> Bool
claveExiste k m = esJust (lookupM k m)

esJust (Just x) = True
esJust Nothing  = False

map1 :: Map String Int
map1 =
	assocM "fede" 123 $
	assocM "ale" 456 $
	assocM "pedro" 12 $
	emptyM

map2 :: Map String Int
map2 = assocM "juan" 534 $
	assocM "pepe" 32 $
	assocM "raul" 43 $
	assocM "pedro" 55 $
	emptyM

numerosDelLoto :: Map Int Bool
numerosDelLoto = 
	assocM 23 True $
	assocM 44 True $
	assocM 55 True $
	assocM 70 True $
	assocM 99 False $
	assocM 77 False $
	assocM 22 False $
	assocM 33 False $
	emptyM

esNumeroGanador :: Int -> Map Int Bool -> Bool
esNumeroGanador n m = 
	esJust (lookupM n m) && valor (lookupM n m)

	-- if esJust (lookupM n m)
	--    then valor (lookupM n m)
	--    else False

--  otra opcion:
-- 	aparece (lookupM n m)

-- aparece :: Maybe Bool -> Bool
-- aparece Nothing  = False
-- aparece (Just b) = b



-- COSTO3 : Lineal
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas [] m     = True
todasAsociadas (k:ks) m = elem k (keys m) && todasAsociadas ks m 



-- COSTO3 : Cuadrático
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap [] = emptyM
listToMap (x:xs) = assocM (fst x) (snd x) (listToMap xs)


-- COSTO3 : Cuadrático, por cada valor hace Lookup que es Lineal 
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m = traerValoresDe (keys m) m 


traerValoresDe :: Eq k => [k] -> Map k v -> [(k, v)]
traerValoresDe []     m = []
traerValoresDe (x:xs) m = (x , (valor (lookupM x m))) : traerValoresDe xs m


-- COSTO3 : Cuadrático, por cada valor hace un assoc que es lineal y un todosLosValores que es lineal  
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq [] 	 = emptyM
agruparEq (x:xs) = assocM (fst x) (todosLosValoresDe (fst x) xs) (agruparEq xs)

todosLosValoresDe :: Eq k => k -> [(k, v)] -> [v]
todosLosValoresDe k [] 		= []
todosLosValoresDe k (x:xs)  = if k == (fst x) then (snd x) : todosLosValoresDe k xs else todosLosValoresDe k xs



-- COSTO3  : Cuadrático claveExiste es Lineal,  

incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar [] m     = m
incrementar (x:xs) m = if (claveExiste x m) then assocM x ((valor (lookupM x m)) + 1) (incrementar xs (deleteM x m)) else incrementar xs m





-- COSTO3 : Cuadrático, O (n*n + n*n)
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = unirMapas (keys m1) m1 m2

unirMapas :: Eq k => [k] -> Map k v -> Map k v -> Map k v
unirMapas (x:[]) m1 m2 = assocM x (valor(lookupM x m1)) m2
unirMapas (x:xs) m1 m2=  assocM x (valor(lookupM x m1)) (unirMapas xs m1 m2)


-- Interfaz
-- emptyM :: Map k v
-- assocM :: Eq k => k -> v -> Map k v -> Map k v
-- lookupM :: Eq k => k -> Map k v -> Maybe v
-- deleteM :: Eq k => k -> Map k v -> Map k v
-- keys :: Map k v -> [k]


indexar :: [a] -> Map Int a
indexar [] 		 = emptyM
indexar xs   = assocM (length xs)  (last xs) (indexar (sinElUltimo xs))

sinElUltimo :: [a] -> [a]
sinElUltimo (x:[]) = []
sinElUltimo (x:xs) = x : sinElUltimo xs


---

ocurrencias :: String -> Map Char Int
ocurrencias [] = emptyM
ocurrencias xs = assocM (last xs) (apariciones (last xs) xs) (ocurrencias (sinElUltimo xs))


apariciones :: Char -> String -> Int
apariciones c [] = 0
apariciones c (s:st) = if c == s then 1 + apariciones c st else apariciones c st

