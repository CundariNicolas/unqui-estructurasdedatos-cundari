--Punto 1 Recursión sobre Listas

sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs 

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs


sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (x:xs) = (x+1) : sucesores xs

conjuncion :: [Bool] -> Bool
conjuncion []       = True
conjuncion (x:xs)   = x && conjuncion xs

disyuncion :: [Bool] -> Bool
disyuncion []       = False
disyuncion (x:xs)   = x || disyuncion xs

aplanar :: [[a]] -> [a]
aplanar []      = []
aplanar (x:xs)  = x ++ aplanar xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece p []     = False
pertenece p (x:xs) = (p==x) || pertenece p xs

apariciones :: Eq a => a -> [a] -> Int
apariciones p []     = 0
apariciones p (x:xs) = if (p==x) then 1 + apariciones p xs else 0 + apariciones p xs

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA m []     = []
losMenoresA m (x:xs) = if(x<m) then x : losMenoresA m xs else losMenoresA m xs

losDeLongitudMayorA :: Int -> [[a]] -> [[a]]
losDeLongitudMayorA n []     = []
losDeLongitudMayorA n (x:xs) = if (longitud x > n) then x : losDeLongitudMayorA n xs else losDeLongitudMayorA n xs

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e     = (e:[])
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

concatenar :: [a] -> [a] -> [a]
concatenar [] ys        = ys
concatenar (x:xs) ys    = x : concatenar xs ys

reversa :: [a] -> [a]
reversa (x:[])  = [x] 
reversa (x:xs)  = reversa xs ++ [x]

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] ys          = ys
zipMaximos xs []          = xs
zipMaximos (x:xs) (y:ys) = if (x>y) then x : zipMaximos xs ys else y : zipMaximos xs ys


elMinimo :: Ord a => [a] -> a
elMinimo (x:[]) = x
elMinimo (x:xs) = min x (elMinimo xs)


-- Punto 2 Recursión sobre Números

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva x = x : cuentaRegresiva (x-1)

repetir :: Int -> a -> [a]
repetir 0 e = []
repetir n e = e : repetir (n-1) e

losPrimeros :: Int -> [a] -> [a]
losPrimeros n [] = []
losPrimeros 0 (x:xs) = x:[]
losPrimeros n (x:xs) = x : (losPrimeros (n-1) xs)

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros n [] = []
sinLosPrimeros 0 (x:xs) = xs
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs


--- Punto 3 Registros

--1

data Persona = ConsP String Int deriving Show

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n []     = []
mayoresA n (x:xs) = if (n < edad x) then x : mayoresA n xs else mayoresA n xs

edad :: Persona -> Int
edad (ConsP n e) = e

--

promedioEdad :: [Persona] -> Int
promedioEdad []     = 0
promedioEdad xs = div (todasLasEdades xs) (longitud xs)

todasLasEdades :: [Persona] -> Int
todasLasEdades []     = 0
todasLasEdades (x:xs) = edad x + todasLasEdades xs

---

elMasViejo :: [Persona] -> Persona
elMasViejo (x:[]) = x
elMasViejo (x:xs) = masGrandeDe x (elMasViejo xs)

masGrandeDe :: Persona -> Persona -> Persona
masGrandeDe a b = if (edad a > edad b) then a else b


data TipoDePokemon = Agua | Fuego | Planta deriving Show

data Pokemon = ConsPokemon TipoDePokemon Int deriving Show

data Entrenador = ConsEntrenador String [Pokemon] deriving Show


cantPokemones :: Entrenador -> Int
cantPokemones (ConsEntrenador x p) = longitud p

cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonesDe t (ConsEntrenador n p) = cantPokesDe t p

cantPokesDe :: TipoDePokemon -> [Pokemon] -> Int
cantPokesDe t []     = 0
cantPokesDe t (x:xs) = if (sonMismoTipo t x) then 1 + cantPokesDe t xs else cantPokesDe t xs

sonMismoTipo :: TipoDePokemon -> Pokemon -> Bool
sonMismoTipo Agua (ConsPokemon Agua _)     = True
sonMismoTipo Fuego (ConsPokemon Fuego _)   = True
sonMismoTipo Planta (ConsPokemon Planta _) = True
sonMismoTipo _ (ConsPokemon _ _) = False

ash = (ConsEntrenador "Ash" [bolbasour, charizard])
brock = (ConsEntrenador "Brock" [bolbasour, charizard,bolbasour,bolbasour, squartle, squartle])

bolbasour = ConsPokemon Planta 100
charizard = ConsPokemon Fuego 100
squartle = ConsPokemon Agua 100


losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan t e1 e2 = if (cantPokemonesDe t e1 > 0 && (cantPokemonesDe (debilContra t) e2)> 0 ) then cantPokemonesDe t e1 else 0


debilContra :: TipoDePokemon -> TipoDePokemon
debilContra Agua   = Planta
debilContra Fuego  = Agua
debilContra Planta = Fuego 


esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador s p) = alMenosUn Fuego p && alMenosUn Agua p && alMenosUn Planta p


alMenosUn :: TipoDePokemon -> [Pokemon] -> Bool
alMenosUn t []    = False
alMenosUn t (x:xs)= sonMismoTipo t x || alMenosUn t xs


--- Punto 3

data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = ConsProyecto String deriving (Eq, Show)
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

proyecto1 = ConsProyecto "Unqflix"
proyecto2 = ConsProyecto "Argentinga"

esteban= Developer Junior proyecto1
pedro= Developer SemiSenior proyecto1
pepe= Developer SemiSenior proyecto1
quico= Developer SemiSenior proyecto1
alan=Management SemiSenior proyecto1
raul= Developer Junior proyecto1
jose= Developer SemiSenior proyecto1
graciela= Developer SemiSenior proyecto1
federico= Developer Senior proyecto1
juan=Management Senior proyecto1

guido= Developer Junior proyecto2
pera= Developer SemiSenior proyecto2
sabo= Developer SemiSenior proyecto2
angel= Developer SemiSenior proyecto2
lean=Management SemiSenior proyecto2
nico= Developer Junior proyecto2
torte= Developer SemiSenior proyecto2
ana= Developer SemiSenior proyecto2
jorge= Developer Senior proyecto2
zoe=Management Senior proyecto2

empresa1 = ConsEmpresa [esteban,pedro,pepe,quico,alan,raul,jose,graciela,federico,juan,guido,pera,sabo,angel,lean,nico,torte,ana,jorge,zoe]


proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa [])     = []
proyectos (ConsEmpresa (x:xs)) = sinRepetidos (proyectoDe x : proyectos (ConsEmpresa xs))

proyectoDe :: Rol -> Proyecto
proyectoDe (Developer s p) = p
proyectoDe (Management x a) = a

sinRepetidos :: [Proyecto] -> [Proyecto]
sinRepetidos []     = []
sinRepetidos (x:xs) = if (estaEn x xs) then sinRepetidos xs else x:sinRepetidos xs

estaEn :: Proyecto -> [Proyecto] -> Bool
estaEn p []     = False
estaEn p (x:xs) = (mismoProyecto p x) || estaEn p xs

mismoProyecto :: Proyecto -> Proyecto -> Bool
mismoProyecto (ConsProyecto s) (ConsProyecto a) = s == a


---


losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior e []     = 0
losDevSenior e (x:xs) = cantidadEnProyecto e x + losDevSenior e xs

cantidadEnProyecto :: Empresa -> Proyecto -> Int
cantidadEnProyecto (ConsEmpresa [])     p = 0
cantidadEnProyecto (ConsEmpresa (x:xs)) p = if (participa x p && esSenior x) then 1 + cantidadEnProyecto (ConsEmpresa xs) p else cantidadEnProyecto (ConsEmpresa xs) p

participa :: Rol -> Proyecto -> Bool
participa r p = mismoProyecto (proyectoDe r) p

esSenior :: Rol -> Bool
esSenior (Developer Senior _) = True
esSenior (Management Senior _) = True
esSenior _ = False

--

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn []     e = 0
cantQueTrabajanEn (x:xs) e = cantidadProy x e + cantQueTrabajanEn xs e

cantidadProy :: Proyecto -> Empresa -> Int
cantidadProy p (ConsEmpresa [])     = 0
cantidadProy p (ConsEmpresa (x:xs)) = if (participa x p) then 1 + cantidadProy p (ConsEmpresa xs) else cantidadProy p (ConsEmpresa xs) 

---

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa roles) = proyectosYCantidad roles (ConsEmpresa roles)

proyectosYCantidad :: [Rol] -> Empresa -> [(Proyecto, Int)]
proyectosYCantidad [] e     = []
proyectosYCantidad (x:xs) e = sinProyectosRepetidos (proyectoYCant x e : proyectosYCantidad xs e)

proyectoYCant :: Rol -> Empresa -> (Proyecto, Int)
proyectoYCant (Developer s p) e    = (p , cantidadProy p e)
proyectoYCant (Management s1 p1) e = (p1 , cantidadProy p1 e)

sinProyectosRepetidos :: [(Proyecto, Int)] -> [(Proyecto, Int)]
sinProyectosRepetidos []     = []
sinProyectosRepetidos (x:xs) = if (estaPar x xs) then sinProyectosRepetidos xs else x:sinProyectosRepetidos xs


estaPar :: (Proyecto, Int) -> [(Proyecto, Int)] -> Bool
estaPar p []    = False
estaPar p (x:xs)= (p==x) || estaPar p xs





