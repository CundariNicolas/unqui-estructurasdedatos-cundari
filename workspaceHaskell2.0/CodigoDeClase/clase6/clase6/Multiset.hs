-- 1. Implementar el tipo abstracto MultiSet utilizando como representación un Map. Indicar los
-- ordenes de complejidad en peor caso de cada función de la interfaz.
-- 2. Reimplementar como usuario de MultiSet la función ocurrencias de ejercicios anteriores,
-- que dado un string cuenta la cantidad de ocurrencias de cada caracter en el string. En este
-- caso el resultado será un multiconjunto de caracteres.
import Map3

multiset1 = addMS "a" $
            addMS "b" $
            addMS "c" $
            addMS "d" $
            addMS "d" $
            addMS "d" $
            addMS "a" $
            addMS "a" $
            addMS "a" $
            addMS "c" $
            addMS "a" $
            addMS "e" $
            emptyMS

multiset2 = addMS "f" $
            addMS "g" $
            addMS "h" $
            addMS "h" $
            addMS "h" $
            addMS "i" $
            addMS "e" $
            addMS "j" $
            addMS "j" $
            addMS "j" $
            addMS "a" $
            addMS "a" $
            addMS "b" $
            emptyMS

--     emptyM,
--     assocM,
--     lookupM,
--     deleteM,
--     keys



data MultiSet a = MST (Map a Int) deriving Show


-- Propósito: denota un multiconjunto vacío.
--costo: constante, O(1)
emptyMS :: MultiSet a
emptyMS = MST (emptyM) 

-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
-- multiconjunto.

--costo: Lineal O(n)
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS e (MST xs) = case lookupM e xs of
                    (Just v) -> MST (assocM e (v+1) xs)
                    Nothing  -> MST (assocM e 1 xs)





-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
-- elemento en el multiconjunto.
--costo : Lineal 
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS a (MST xs) = case lookupM a xs of
                            (Just v) -> v
                            Nothing  -> error "No hay valores" 




-- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
-- ambos multiconjuntos.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
unionMS (MST xs) (MST ys) = MST (unirMaps (keys xs) xs ys)


unirMaps :: Ord k => [k] -> Map k Int -> Map k Int -> Map k Int
unirMaps [] xs ys       = ys
unirMaps (a:as) xs ys   = if (esJust (lookupM a ys)) then assocM a ((valor (lookupM a xs)) + (valor (lookupM a ys))) (unirMaps as xs ys) else assocM a (valor (lookupM a xs)) (unirMaps as xs ys)

esJust :: Maybe v -> Bool
esJust (Just v) = True
esJust Nothing = False

valor :: Maybe v -> v
valor (Just v) = v



-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
-- multiconjuntos tienen en común.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersectionMS (MST xs) (MST ys) = MST (intersectionL (keys xs) xs ys)

intersectionL :: Ord k => [k] -> Map k Int -> Map k Int -> Map k Int
intersectionL [] xs ys       = emptyM
intersectionL (a:as) xs ys   = if (esJust (lookupM a ys)) then assocM a ((valor (lookupM a xs)) + (valor (lookupM a ys))) (intersectionL as xs ys) else intersectionL as xs ys

-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
-- su cantidad de ocurrencias.
multiSetToList :: Ord a => MultiSet a -> [(a, Int)]
multiSetToList (MST ms) = mapToList (keys ms) ms

mapToList :: Ord k => [k] -> Map k Int -> [(k, Int)]
mapToList [] mapsito     = []
mapToList (k:ks) mapsito = (k, valor (lookupM k mapsito)) : mapToList ks mapsito
