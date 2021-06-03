ejNavesPrueba = N (arbolSectores1)

arbolSectores1 = (NodeT sector10 (NodeT sector20 (NodeT sector40 (EmptyT)(EmptyT))(NodeT sector50 (EmptyT)(EmptyT)))(NodeT sector30 (NodeT sector60 (EmptyT)(EmptyT))(NodeT sector70 (EmptyT)(EmptyT))))

--




sector10 = (S "10" [LanzaTorpedos] ["Jose"])

sector20 = (S "20" [(Motor 100)] ["Anakin Skywalker"])
sector30 = (S "30" [(Almacen [Comida])] ["Darth Vader"])
sector40 = (S "40" [(Almacen [Oxigeno])] ["Chuwaka", "Roberto"])
sector50 = (S "50" [LanzaTorpedos] ["Chuck Norris" , "Roberto"])
sector60 = (S "60" [(Motor 200)] ["Harrison Ford", "Roberto"])
sector70 = (S "70" [(Almacen [Comida])] ["Roberto"])

componentes20 = [LanzaTorpedos, (Motor 100), (Almacen [Torpedo,Oxigeno,Torpedo,Comida,Oxigeno,Oxigeno,Oxigeno])]

tripulante20 = "Josesito El Agregadito"

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show

data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show

data Sector = S SectorId [Componente] [Tripulante] 

type SectorId = String

type Tripulante = String

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

data Nave = N (Tree Sector)


--- 1) 
--Propósito: Devuelve todos los sectores de la nave.                
sectores :: Nave -> [SectorId]
sectores (N sec) = todosSectoresDeNave sec

--Auxiliar
todosSectoresDeNave :: Tree Sector -> [SectorId]
todosSectoresDeNave EmptyT = []
todosSectoresDeNave (NodeT s si sd) = idSector s ++ todosSectoresDeNave sd ++ todosSectoresDeNave si


--Auxiliar2
idSector :: Sector -> [SectorId]
idSector (S id _ _) = (id:[])



---2)
-- Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
-- el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N sec) = sumarPoder sec


--Auxiliar
sumarPoder :: Tree Sector -> Int
sumarPoder EmptyT = 0
sumarPoder (NodeT s si sd) = poderSector s + sumarPoder si + sumarPoder sd


--
poderSector :: Sector -> Int
poderSector (S id c t) = buscarPropulsion c

--
buscarPropulsion :: [Componente] -> Int
buscarPropulsion [] = 0
buscarPropulsion (x:xs) = propulsion x + buscarPropulsion xs 

--

propulsion :: Componente -> Int
propulsion (Motor p) = p
propulsion _ = 0 

--3)

-- Propósito: Devuelve todos los barriles de la nave.
barriles :: Nave -> [Barril]
barriles (N sec) = barriles' sec

barriles' :: Tree Sector -> [Barril]
barriles' EmptyT = []
barriles' (NodeT s si sd) = barrilesDeSec s ++ barriles' si ++ barriles' sd

barrilesDeSec :: Sector -> [Barril]
barrilesDeSec (S id c t) = barrilesEnComp c

barrilesEnComp :: [Componente] -> [Barril]
barrilesEnComp [] = []
barrilesEnComp (x:xs) = barrilesDe x ++ barrilesEnComp xs

barrilesDe :: Componente -> [Barril]
barrilesDe (Almacen x) = x
barrilesDe _ = []



--4)
-- Propósito: Añade una lista de componentes a un sector de la nave.
-- Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N sec) = N (agregarASector' cs id sec)

agregarASector' :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASector' cs id EmptyT = EmptyT
agregarASector' cs id (NodeT s si sd) = (NodeT (agregarComp cs id s) (agregarASector' cs id si) (agregarASector' cs id sd))


agregarComp :: [Componente] -> SectorId -> Sector -> Sector
agregarComp cs id (S idS c t) = if (id == idS) then (S idS (c ++ cs) t) else (S idS c t)  


--5)
-- Propósito: Incorpora un tripulante a una lista de sectores de la nave.
-- Precondición: Todos los id de la lista existen en la nave.

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t ids (N sec) = (N (agregarTrip t ids sec))

agregarTrip :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
agregarTrip t ids EmptyT = EmptyT
agregarTrip t ids (NodeT s si sd) = (NodeT (agregar t ids s) (agregarTrip t ids si) (agregarTrip t ids sd))

agregar :: Tripulante -> [SectorId] -> Sector -> Sector
agregar t [] (S id c ts) = (S id c ts)
agregar t (x:xs) (S id c ts) = if (x == id) then agregar t xs (S id c (t:ts)) else agregar t xs (S id c ts)


--6) 
--Propósito: Devuelve los sectores en donde aparece un tripulante dado.
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N sec) = sectoresDe t sec

sectoresDe :: Tripulante -> Tree Sector -> [SectorId]
sectoresDe t EmptyT = []
sectoresDe t (NodeT s si sd) = aparece t s ++ sectoresDe t si ++ sectoresDe t sd

aparece :: Tripulante -> Sector -> [SectorId]
aparece t (S id c ts) = if(elem t ts) then (id:[]) else []



--7)
--Propósito: Devuelve la lista de tripulantes, sin elementos repetidos.
tripulantes :: Nave -> [Tripulante]
tripulantes (N sec) = sinRepetidos (listaDeTrip sec)

listaDeTrip :: Tree Sector -> [Tripulante]
listaDeTrip EmptyT = []
listaDeTrip (NodeT s si sd) = tripulantesSector s ++ listaDeTrip si ++ listaDeTrip sd

tripulantesSector :: Sector -> [Tripulante]
tripulantesSector (S id c ts) = ts


--

sinRepetidos :: [Tripulante] -> [Tripulante]
sinRepetidos [] = []
sinRepetidos (x:xs) = if (elem x xs) then sinRepetidos xs else (x:sinRepetidos xs)


