data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show

data Ingrediente = Salsa
                 | Queso
                 | Jamon
                 | Aceitunas Int deriving Show

pruebaPizza = Capa Queso (Capa Jamon (Capa (Aceitunas 20)(Capa Salsa (Capa Salsa (Capa Queso Prepizza)))))

pruebaPizza2 = Capa Queso (Capa Queso (Capa Salsa (Capa Salsa (Capa Salsa (Capa Queso Prepizza)))))

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p



armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (x:xs) = (Capa x (armarPizza xs)) 


sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if (esJamon i) then sacarJamon p else (Capa i (sacarJamon p))

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

tieneSoloSalsaYQueso :: Pizza -> Bool
--Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso Prepizza  = True
tieneSoloSalsaYQueso (Capa i p)= esSalsaOQueso i && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _     = False



duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza               = Prepizza
duplicarAceitunas (Capa (Aceitunas x) p) = Capa (Aceitunas (x*2)) (duplicarAceitunas p)
duplicarAceitunas (Capa i p) = Capa i (duplicarAceitunas p)     

--Recorre cada ingrediente y si es aceitunas duplica su cantidad


cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (x:xs) = (cantidadDeCapas x, x) : cantCapasPorPizza xs  

--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.


------------------------------------------------------------------------------------------------------------------------------


data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

cofre1 = Cofre [Chatarra]
cofre2 = Cofre [Tesoro]
mapaTesoro = Bifurcacion cofre1 (Bifurcacion cofre1 (Bifurcacion cofre1 (Fin cofre1)(Bifurcacion cofre1 (Bifurcacion cofre1 (Fin cofre1)(Bifurcacion cofre1 (Fin cofre1)(Fin cofre1)))(Fin cofre1)))(Bifurcacion cofre2 (Fin cofre1)(Fin cofre1)))(Bifurcacion cofre1 (Bifurcacion cofre1 (Fin cofre1)(Fin cofre1))(Bifurcacion cofre1 (Fin cofre1)(Fin cofre1)))
mapaTesoro1 = Bifurcacion cofre1 
                                (Bifurcacion cofre2 (Fin cofre1) 
                                                    (Bifurcacion cofre2 (Fin cofre1) (Fin cofre2)))
                                (Bifurcacion cofre1 (Fin cofre1) (Fin cofre2))

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2
--Indica si hay un tesoro en alguna parte del mapa.

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre []) = False
hayTesoroEnCofre (Cofre xs) = hayAlgunTesoro xs

hayAlgunTesoro :: [Objeto] -> Bool
hayAlgunTesoro [] = False
hayAlgunTesoro (x:xs) = esTesoro x || hayAlgunTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False


--Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Fin c)                   = hayTesoroEnCofre c
hayTesoroEn [] (Bifurcacion c _ _)       = hayTesoroEnCofre c
hayTesoroEn (d:ds) (Fin _)               = error "Instrucciones inválidas"
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = case d of
                                                    Izq -> hayTesoroEn ds m1
                                                    Der -> hayTesoroEn ds m2



--Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin c)               = if (hayTesoroEnCofre c) then [] else error "Debe haber un tesoro en el mapa"
caminoAlTesoro (Bifurcacion c m1 m2) = if (hayTesoroEnCofre c) then [] else
                                                                           if (hayTesoro m1) then Izq: caminoAlTesoro m1 else Der: caminoAlTesoro m2



--Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c)                  = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2)    = if (largoMapa m1 > largoMapa m2) then Izq: caminoDeLaRamaMasLarga m1 else Der: caminoDeLaRamaMasLarga m2

largoMapa :: Mapa -> Int
largoMapa (Fin c) = 0
largoMapa (Bifurcacion c m1 m2) = 1 + largoMapa m1 + largoMapa m2


profundidadMapa :: Mapa -> Int
profundidadMapa (Fin c) = 0
profundidadMapa (Bifurcacion c m1 m2) = 1 + if (largoMapa m1 > largoMapa m2) then profundidadMapa m1 else profundidadMapa m2



-- Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel mapa =  todosLosTesoros (profundidadMapa mapa) mapa

todosLosTesoros :: Int -> Mapa -> [[Objeto]]
todosLosTesoros 0 m = (tesorosEnNivel 0 m):[]
todosLosTesoros n m = tesorosEnNivel n m : todosLosTesoros (n-1) m

tesorosEnNivel :: Int -> Mapa -> [Objeto]
tesorosEnNivel 0 (Fin c)               = cofreDe c
tesorosEnNivel 0 (Bifurcacion c m1 m2) = cofreDe c
tesorosEnNivel n (Fin c)               = []
tesorosEnNivel n (Bifurcacion c m1 m2) = tesorosEnNivel (n-1) m1 ++ tesorosEnNivel (n-1) m2

cofreDe :: Cofre -> [Objeto]
cofreDe (Cofre x) = x

--Devuelve todos lo caminos en el mapa.
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin c)= []
todosLosCaminos (Bifurcacion c (Fin a) (Fin b)) = (agregarDir Izq []) ++ (agregarDir Der [])
todosLosCaminos (Bifurcacion c m1 m2) = agregarDir Izq (todosLosCaminos m1) ++ agregarDir Der (todosLosCaminos m2)


agregarDir :: Dir -> [[Dir]] -> [[Dir]]
agregarDir d []       = (d:[]):[]
agregarDir d (xs:xss) = (d:xs) : agregarDir d xss

