--Punto 1 Números Enteros

-- Parte 1
-- a
sucesor :: Int -> Int
sucesor x = x + 1

-- b
sumar :: Int -> Int -> Int
sumar x y = x + y
-- c 
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y , mod x y)

-- d 
maxDelPar :: (Int, Int) -> Int
maxDelPar (x,y) = max x y


-- Parte 2
-- sumar (sucesor 4) (maxDelPar (divisionYResto 10 2))
-- sucesor (maxDelPar ((sumar 4 5), 6))
-- maxDelPar (divisionYResto 20 2, sucesor (sumar 1 2))
-- maxDelPar ((sumar 5 5) (maxDelPar((sucesor 1), 2)


-- Punto 2 Tipos Enumerativos
-- Parte 1 
data Dir = Norte | Sur | Este | Oeste deriving Show

--a
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

--b 
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False


-- c
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Sur = Oeste
siguiente Oeste = Norte
siguiente Este = Sur
-- La función se volvería parcial porque no podría contemplar el caso siguiente a Oeste. Tiraría una excepción.


-- Parte 2



-- a
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

-- b 

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

-- c
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Martes Lunes = True
vieneDespues Miercoles Martes = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves = True
vieneDespues Sabado Viernes = True
vieneDespues Domingo Sabado = True
vieneDespues Lunes Sabado = True
vieneDespues _ _ = False


-- d
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True


-- Parte 3 

--a
negar :: Bool -> Bool
negar True = False
negar False = True


--b 

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

-- c
and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False

-- d 
or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True



-- Punto 3

-- Parte 1 

data Persona = ConsP String Int deriving Show


nombre :: Persona -> String
nombre (ConsP n e) = n

edad :: Persona -> Int
edad (ConsP n e) = e

crecer :: Persona -> Persona
crecer (ConsP n e) = (ConsP n (e+1))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre n (ConsP nm e) = (ConsP n e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra pu pd = (edad pu) > (edad pd)

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor pu pd = if (esMayorQueLaOtra pu pd) then pu else pd

data TipoDePokemon = Agua | Fuego | Planta deriving Show

data Pokemon = ConsPoke TipoDePokemon Int deriving Show

data Entrenador = ConsE String Pokemon Pokemon deriving Show

superaA :: TipoDePokemon -> TipoDePokemon -> Bool
superaA Agua Fuego = True
superaA Fuego Planta = True
superaA Planta Agua = True
superaA _ _ = False

cantidadDePokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonesDe Agua (ConsE _ (ConsPoke Agua _) (ConsPoke Agua _)) = 2
cantidadDePokemonesDe Agua (ConsE _ (ConsPoke Agua _) (ConsPoke _ _)) = 1
cantidadDePokemonesDe Agua (ConsE _ (ConsPoke _ _) (ConsPoke _ _)) = 0
cantidadDePokemonesDe Fuego (ConsE _ (ConsPoke Fuego _) (ConsPoke Fuego _)) = 2
cantidadDePokemonesDe Fuego (ConsE _ (ConsPoke Fuego _) (ConsPoke _ _)) = 1
cantidadDePokemonesDe Fuego (ConsE _ (ConsPoke _ _) (ConsPoke _ _)) = 0
cantidadDePokemonesDe Planta (ConsE _ (ConsPoke Planta _) (ConsPoke Planta _)) = 2
cantidadDePokemonesDe Planta (ConsE _ (ConsPoke Planta _) (ConsPoke _ _)) = 1
cantidadDePokemonesDe Planta (ConsE _ (ConsPoke _ _) (ConsPoke _ _)) = 0



juntarPokemones :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemones ((ConsE _ x y), (ConsE _ z v)) = (x:y:z:v:[])


--- Parte 4
-- Funciones Polimórficas


--a
loMismo :: a -> a 
loMismo x = x

-- b
siempreSiete :: a -> Int
siempreSiete x = 7

-- c
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Son variables de tipo. Las variables pueden ser de cualquier tipo, iguales o no y la función funciona igual.
-- Son polimórficas porque no importa el tipo de dato que reciban como argumento, siempre actúan de la misma manera.


-- Parte 5
-- Pattern Matching sobre Listas

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia xs = False

elPrimero :: [a] -> a
elPrimero [] = error "Debe tener al menos un elemento"
elPrimero (x:xs) = x

sinElPrimero :: [a] -> [a]
sinElPrimero [] = error "Debe tener al menos un elemento"
sinElPrimero (x:xs) = xs

splitHead :: [a] -> (a, [a])
splitHead [] = error "Debe haber al menos un elemento"
splitHead (x:xs) = (x, xs)