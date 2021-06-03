module Organizador
(
  Organizador,
  nuevo,
  agregarPrograma,
  todosLosProgramas,
  autoresDe,
  programasDe,
  programaronJuntas,
  nroProgramasDePersona,
  Persona,
  Checksum
)
where

import Set2
import Map2


data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
  deriving Show

type Checksum = String
type Persona = String

-- Propósito: Un organizador vacío.
-- Eficiencia: O(1)
nuevo :: Organizador
nuevo = MkO emptyM emptyM 


-- Propósito: Agrega al organizador un programa con el Checksum indicado; 
-- el conjunto es el conjunto de personas autores de dicho programa.
-- Precondición: el identificador del programa que se agrega no fue usado 
-- previamente en el organizador, y el Set de personas no está vacío.
-- Eficiencia: no hay ninguna garantía de eficiencia.
-- O(log C + P log P') donde
-- C es cantidad de programas actuales
-- P es cantidad autores del nuevo programa y
-- P' es cantidad de autores registrados al momento
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO porPrograma porPersona) prog personas =
  MkO 
    -- log C donde C es programas actuales
    (assocM prog personas porPrograma) 
    -- P * log P'
    (assocTodos (setToList personas) prog porPersona)


-- O(P * log P')  donde P es lista de personas y P' es el tamanio del set
assocTodos :: 
  [Persona] -> 
  Checksum -> 
  Map Persona (Set Checksum) -> 
  Map Persona (Set Checksum)
assocTodos [] programa porPersona = porPersona
assocTodos (p:ps) programa porPersona = 
  let programasDeP = emptyIfNothing (lookupM p porPersona) in
    assocM p 
      (addS programa programasDeP) 
      (assocTodos ps programa porPersona)

-- O(1)
emptyIfNothing :: Maybe (Set a) -> Set a
emptyIfNothing Nothing = emptyS
emptyIfNothing (Just s) = s 





-- Propósito: denota una lista con todos y cada uno de los códigos 
-- identificadores de programas del organizador.
-- Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el 
-- organizador.
todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas (MkO porPrograma porPersona) = 
  keys porPrograma

-- Propósito: denota el conjunto de autores que aparecen en un programa 
-- determinado.
-- Precondición: el Checksum debe corresponder a un programa del organizador.
-- Eficiencia: O(log C) en peor caso, donde C es la cantidad total de 
-- programas del organizador.
autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe (MkO porPrograma porPersona) programa =
  emptyIfNothing (lookupM programa porPrograma)

-- Propósito: denota el conjunto de programas en los que participó una 
-- determinada persona.
-- Precondición: la persona debe existir en el organizador.
-- Eficiencia: O(log P ) en peor caso, donde P es la cantidad total de 
-- personas del organizador.
programasDe :: Organizador -> Persona -> Set Checksum
programasDe (MkO porPrograma porPersona) persona =
  emptyIfNothing (lookupM persona porPersona)

-- Propósito: dado un organizador y dos personas, denota verdadero si ambas 
-- son autores de algún software en común.
-- Precondición: las personas deben ser distintas.
-- Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de 
-- personas distintas que aparecen en todos los programas del organizador, 
-- y C la cantidad total de programas.
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
programaronJuntas org p1 p2 = 
  0 < (sizeS $ enComun org p1 p2)

-- O(cP1 log cP2)
enComun :: Organizador -> Persona -> Persona -> Set Checksum
enComun org p1 p2 = 
  -- O(cP1 log cP2)
  intersection 
    -- O(log P) donde P es cantidad de Autores
    (programasDe org p1) 
    -- O(log P) donde P es cantidad de Autores
    (programasDe org p2)


-- Propósito: dado un organizador y una persona, denota la cantidad de 
-- programas distintos en los que aparece.
-- Eficiencia: O(log P ) en peor caso, donde P es la cantidad de personas 
-- del organizador.
nroProgramasDePersona :: Organizador -> Persona -> Int
nroProgramasDePersona org p = sizeS (programasDe org p)
