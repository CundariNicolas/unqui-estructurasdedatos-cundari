data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)


-- Invariantes de representación:
-- Todos los tripulantes de los sectores están en la heap
-- El par de Sector e Int es el sector con más tripulantes de la nave e Int no puede ser menor a 0
-- 

-- Propósito: Crea una nave con todos esos sectores sin tripulantes.
-- Precondición: la lista de sectores no está vacía
-- Costo: O(S log S) siendo S la cantidad de sectores de la lista.
naveVacia :: [Sector] -> Nave
naveVacia []     = MkN m h s
naveVacia (s:ss) = MkN (assocM s (emptyS)) emptyH (s, 0) 


-- Propósito: Obtiene los tripulantes de un sector.
-- Costo: O(log S) siendo S la cantidad de sectores.
tripulantesDe :: Sector -> Nave -> Set Tripulante
tripulantesDe s (MkN map heap par) = fromJust (lookupM map s)


-- Costo : Constante
fromJust :: Maybe v -> v
fromJust (Just s) = s
fromJust Nothing = emptyS



-- Propósito: Denota los sectores de la nave
-- Costo: O(S) siendo S la cantidad de sectores.
sectores :: Nave -> [Sector]
sectores (MkN map heap par) = domM map 


-- Propósito: Denota el tripulante con mayor rango.
-- Precondición: la nave no está vacía.
-- Costo: O(1).
conMayorRango :: Nave -> Tripulante
conMayorRango (MkN map heap par) = findMin heap 


-- Propósito: Denota el sector de la nave con más tripulantes.
-- Costo: O(1).
conMasTripulantes :: Nave -> Sector
conMasTripulantes (MkN map heap par) = fst par

-- Propósito: Denota el conjunto de tripulantes con dicho rango.
-- Costo: O(P log P) siendo P la cantidad de tripulantes.
conRango :: Rango -> Nave -> Set Tripulante
conRango r (MkN map heap par) = if isEmptyH heap 
                                then emptyS 
                                else if (rango (findMin heap) == r) 
                                    then addS (findMin heap) (conRango r (MkN map (deleteMin heap) par )) 
                                    else conRango r (MkN map (deleteMin heap) par )


-- Propósito: Devuelve el sector en el que se encuentra un tripulante.
-- Precondición: el tripulante pertenece a la nave.
-- Costo: O(S log S log P) siendo S la cantidad de sectores y P la cantidad de tripulantes.
sectorDe :: Tripulante -> Nave -> Sector
sectorDe t (MkN map heap par) = buscarEn (domM map) t (MkN map heap par)

buscarEn :: [Sector] -> Tripulante -> Nave -> Sector
buscarEn [] t n    = error "No cumple con la precondición"
buscarEn (s:ss) t n= if (belongs t (tripulantesDe s n )) then s else buscarEn ss t n




-- Propósito: Agrega un tripulante a ese sector de la nave.
-- Precondición: El sector está en la nave y el tripulante no.
-- Costo: No hay datos (justifique su elección).
agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
agregarTripulante t s (MkN map heap par) = (MkN (buscarYActualizar s t map (MkN map heap par)) (insertH t heap) (actualizarSectorMax par map s))


-- Reemplaza el k y v de ese sector con el tripulante agregado en el set obtenido de ese sector

buscarYActualizar :: Sector -> Tripulante -> Map Sector (Set Tripulante) -> Nave -> Map Sector (Set Tripulante)
buscarYActualizar s t map nave = assocM s ( addS t (tripulantesDe s nave)) map


actualizarSectorMax :: (Sector, Int) -> Map Sector (Set Tripulante) -> Sector -> (Sector, Int)
actualizarSectorMax (sectorActual, cant) map s = if (cant > sizeS (tripulantesDe s map)) then (SectorActual, cant) else (s, sizeS (tripulantesDe s map))









------------------ Funciones como USUARIO 

-- Propósito: Denota los tripulantes de la nave
--a) 
tripulantes :: Nave -> Set Tripulante
tripulantes nave = tripulantes' (sectores nave) nave


tripulantes' :: [Sector] -> Nave -> Set Tripulante
tripulantes' [] n     = emptyS
tripulantes' (s:ss) n = union (tripulantesDe s n) (tripulantes' ss n)



-- Propósito: Elimina al tripulante de la nave.
-- Pista: Considere reconstruir la nave sin ese tripulante.
-- Opcional (Bonus): 
bajaDeTripulante :: Tripulante -> Nave -> Nave
bajaDeTripulante t n = reconstruirNaveSinT t n (sectores n)


reconstruirNaveSinT :: Tripulante -> Nave -> [Sector] -> Nave
reconstruirNaveSinT t n sectores = agregarTodos t n sectores (naveVacia sectores)


agregarTodos :: Tripulante -> Nave -> [Sector] -> Nave -> Nave
agregarTodos t n [] naveSoloSectores = naveSoloSectores
agregarTodos t n (x:xs) naveVacia = agregarTripulantesDeSectorSin t x (set2list(tripulantesDe x n)) (agregarTodos t n xs naveVacia)

agregarTripulantesDeSectorSin :: Tripulante -> Sector -> [Tripulante] -> Nave -> Nave
agregarTripulantesDeSectorSin t sec []     nave = nave
agregarTripulantesDeSectorSin t sec (x:xs) nave = if (t == x) then (agregarTripulantesDeSectorSin t sec xs nave) else agregarTripulante x sec (agregarTripulantesDeSectorSin t sec xs nave)