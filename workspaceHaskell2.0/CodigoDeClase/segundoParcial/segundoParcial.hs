-- Parcial 2 - Estructuras de Datos - Nicolas Cundari 


-- a) Dar invariantes de representación válidos según la descripción de la estructura.
--  La cantidad de elementos del Map Evidencia y Nombre debe ser igual que el Int del constructor(Cantidad de evidencia).
--  La cantidad de nombres con los que se relaciona la evidencia no puede ser mayor que la cantidad de nombres del map que relaciona
--  nombres con personas


data Investigacion = ConsI (Map Nombre Persona)
                           (Map Evidencia [Nombre])
                           (PriorityQueue Persona)
                            Int


-- b)
-- Propósito: crea una investigación sin datos.
-- Eficiencia: O(1)
comenzarInvestigacion :: Investigacion
comenzarInvestigacion = ConsI emptyM emptyM emptyPQ 0


-- c)
-- Propósito: devuelve la cantidad de eviencia ingresada.
-- Eficiencia: O(1)
cantEvidenciaIngresada :: Investigacion -> Int
cantEvidenciaIngresada (ConsI _ _ _ e) = e

-- d)
-- Propósito: devuelve la evidencia ingresada.
-- Eficiencia: O(N) porque domM es O(K)
evidenciaIngresada :: Investigacion -> [Evidencia]
evidenciaIngresada (ConsI _ m2 _ _) = domM m2


-- e)
-- Propósito: devuelve los nombres de personas ingresadas.
-- Eficiencia: O(N) porque domM es O(K)
nombresIngresados :: Investigacion -> [Nombre]
nombresIngresados (ConsI m1 _ _ _) = domM m1


-- f)
-- Propósito: indica si la investigación posee al menos una persona con 5 evidencias en su contra.
-- Eficiencia: O(1) porque maxPQ y cantEvidencia es constante. 
casoCerrado :: Investigacion -> Bool
casoCerrado (ConsI _ _ pq _) = cantEvidencia (maxPQ pq) >= 5


--g)
-- Propósito: indica si esa persona tiene al menos una evidencia en su contra.
-- Nota: la persona puede no existir.
-- Eficiencia: O(log N) porque deleteMaxPQ es O(log P)
esSospechoso :: Nombre -> Investigacion -> Bool
esSospechoso n (ConsI m1 m2 pq cantE) = if (isEmptyPQ pq) then False 
                                                          else if (nombre (maxPQ pq) == n ) 
                                                              then cantEvidencia (maxPQ pq) >= 1 
                                                              else esSospechoso n (ConsI m1 m2 (deleteMaxPQ pq) cantE)




-- Propósito: devuelve a las personas con cero evidencia en su contra.
-- Eficiencia: O(N log N) porque por cada persona ejecuta esSospechoso que es log P
-- h)
posiblesInocentes :: Investigacion -> [Persona]
posiblesInocentes inv = posiblesInocentes' (nombresIngresados inv) inv

-- Auxiliar: proposito: devuelve una lista de personas que no son sospechosas en una investigación dada
posiblesInocentes' :: [Persona] -> Investigacion -> [Persona]
posiblesInocentes' [] inv     = []
posiblesInocentes' (p:ps) inv =  if (esSospechoso (nombre p) inv) then posiblesInocentes' ps inv else p : posiblesInocentes' ps inv 


-- i)
-- Propósito: ingresa a personas nuevas a la investigación (mediante sus nombres), sin evidencia en su contra.
-- Precondición: las personas no existen en la investigación y no hay nombres repetidos.
-- Eficiencia: O(N log N) porque por cada N (nombre), hace assocM e insertPQ que son O (log N)
ingresarPersonas :: [Nombre] -> Investigacion -> Investigacion
ingresarPersonas [] inv = inv 
ingresarPersonas (n:ns) (ConsI m1 m2 pq cantE) = ingresarPersonas ns (ConsI (assocM n (crearP n) m1) m2 (insertPQ (crearP n) pq) cantE)

-- j)
-- Propósito: asocia una evidencia a una persona dada.
-- Precondición: la evidencia aún no está asociada a esa persona.
-- Nota: la persona y la evidencia existen, pero NO están asociadas.
-- Eficiencia: O(N log N) porque agregarEvid es N log N
-- 
ingresarEvidencia :: Evidencia -> Nombre -> Investigacion -> Investigacion
ingresarEvidencia evid nom (ConsI m1 m2 pq cantE) = ConsI (buscarYAgregar evid nom m1) (buscarEvid evid nom m2) (agregarEvid evid nom pq) cantE

--Auxiliares
-- Propósito: actualiza el mapa, agregando la evidencia a la persona con la que está relacionada ese nombre
---- Costo: O (log N)
buscarYAgregar :: Evidencia -> Nombre -> Map Nombre Persona -> Map Nombre Persona
buscarYAgregar evid nom m1 = assocM nom (agregarEvidencia evid (fromJust (lookupM nom m1))) m1

fromJust :: Maybe v -> v
fromJust (Just v) = v

----Propósito : Busca en el map la evidencia y la actualiza agregando el nombre y volviendo a asociar esa key con el nombre agregado
---- Costo: O (log N)

--- OBSERVACIÓN : Si tengo que verificar que existe la evidencia en el mapa se haría lineal al buscar entre
--- las keys del Map la evidencia que busco. Intento hacerlo con un lookupM suponiendo que ese lookupM devuelve nothing incluso si la clave no existe
--- para mantener la eficiencia <-------------------------

buscarEvid :: Evidencia -> Nombre -> Map Evidencia [Nombre] -> Map Evidencia [Nombre]
buscarEvid evid nom m2 = if existeEvid evid m2 
                                    then assocM evid (agregarNom nom (fromJust (lookupM evid m2))) m2 
                                    else assocM evid [nom] m2


-- Propósito: Indica si existe esa evidencia en el map
existeEvid :: Evidencia -> Map Evidencia [Nombre] -> Bool
existeEvid evid m2 = case lookupM evid m2 of
                        (Just v) -> True
                        Nothing -> False

--- Proposito: pone un nombre delante de toda la lista de nombres
--- Costo: Constante
agergarNom :: Nombre -> [Nombre] -> [Nombre]
agregarNom nom xs = (nom:xs)


--- Propósito: busca a la persona en la PQ y actualiza sus evidencias agregando la última si es que está. Y si no está, lo agrega con esa única evidencia
-- Eficiencia : En el peor caso es O(N log N) porque recorre toda la PQ y hace una inserción y borrado en la PQ 
agregarEvid :: Evidencia -> Nombre -> PriorityQueue Persona -> PriorityQueue Persona
agregarEvid evid nom pq = if isEmptyPQ pq then insertPQ (agregarEvidencia evid (crearP nom)) pq 
                                          else if (nombre (maxPQ pq) == nom) 
                                               then insertPQ (agregarEvidencia evid (maxPQ pq)) (deleteMaxPQ pq)
                                               else insertPQ (maxPQ pq) (agregarEvid evid nom (deleteMaxPQ pq))




--Usuario
--Implementar las siguientes funciones como usuario del tipo Investigacion, indicando el costo obtenido:


-- Propósito: Comienza una investigación con una lista de nombres sin evidencia.
-- k)
comenzarConPersonas :: [Nombre] -> Investigacion
comenzarConPersonas ns = ingresarPersonas ns (comenzarInvestigacion)


--Propósito: Indica si las personas en la investigación son todas inocentes.
-- Costo : (N log N) por que posibles inocentes es N log N
todosInocentes :: Investigacion -> Bool
todosInocentes inv = length (posiblesInocentes inv)  == length (nombresIngresados inv)

-- m)
--Propósito: Indica si la evidencia en la lista es suficiente para cerrar el caso.
terminaCerrado :: [(Evidencia, Nombre)] -> Investigacion -> Bool
terminaCerrado xs inv = casoCerrado (investigar xs inv)

investigar :: [(Evidencia, Nombre)] -> Investigacion -> Investigacion
investigar [] inv     = inv
investigar (x:xs) inv = ingresarEvidencia (fst x) (snd x) (ingresarPersonas ((snd x):[]) (investigar xs inv))



-- Representación
-- n) Dar una posible representación para el tipo Persona, de manera de que se pueda cumplir con el orden dado para cada
-- operación de la interfaz, pero sin implementarlas.

type cantidadEvidencia = Int
type Nombre = String

data Persona = consP Nombre [Evidencia] cantidadEvidencia


---------------------------------------------------------------------------------------------------------------------------------------------------------------






