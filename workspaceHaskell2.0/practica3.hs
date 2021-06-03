
data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show



celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))

nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia           = 0
nroBolitas c (Bolita color celda) = if (esMismoColor c color) then 1 + nroBolitas c celda else nroBolitas c celda

esMismoColor :: Color -> Color -> Bool
esMismoColor Azul Azul = True
esMismoColor Rojo Rojo = True
esMismoColor _ _ = False



poner :: Color -> Celda -> Celda
poner c celda = (Bolita c celda)

sacar :: Color -> Celda -> Celda
sacar c CeldaVacia           = CeldaVacia
sacar c (Bolita color celda) = if (esMismoColor c color) then celda else (Bolita color (sacar c celda)) 


ponerN :: Int -> Color -> Celda -> Celda
ponerN 1 c celda = poner c celda
ponerN n c celda = poner c (ponerN (n-1) c celda) 


--- Camino


data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada camino) = hayTesoro camino
hayTesoro (Cofre xs c)  = contieneTesoro xs || hayTesoro c

contieneTesoro :: [Objeto] -> Bool
contieneTesoro [] = False
contieneTesoro (x:xs) = algunTesoro x || contieneTesoro xs

algunTesoro :: Objeto -> Bool
algunTesoro Tesoro = True
algunTesoro _      = False


-- PRECONDICIÓN: DEBE HABER AL MENOS UN TESORO
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Nada camino) = 1 + pasosHastaTesoro camino
pasosHastaTesoro (Cofre xs camino) = if (contieneTesoro xs) then 0 else 1 + pasosHastaTesoro camino 

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 1 camino            = hayTesoroAhi camino 
hayTesoroEn n (Nada camino)     = hayTesoroEn (n-1) camino
hayTesoroEn n (Cofre xs camino) = hayTesoroEn (n-1) camino


hayTesoroAhi :: Camino -> Bool
hayTesoroAhi (Cofre xs c) = contieneTesoro xs
hayTesoroAhi _            = False




alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = n <= cantidadTesoros camino

cantidadTesoros :: Camino -> Int
cantidadTesoros Fin               = 0
cantidadTesoros (Nada c)          = cantidadTesoros c
cantidadTesoros (Cofre xs camino) = if(contieneTesoro xs) then 1 + cantidadTesoros camino else cantidadTesoros camino



cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre d h camino = tesorosEntre d (h-d) camino


tesorosEntre :: Int -> Int -> Camino -> Int
tesorosEntre 1 h camino            = tesorosHasta h camino
tesorosEntre d h (Nada camino)     = tesorosEntre (d-1) h camino 
tesorosEntre d h (Cofre xs camino) = tesorosEntre (d-1) h camino

tesorosHasta :: Int -> Camino -> Int
tesorosHasta 0 (Nada c)          = 0
tesorosHasta 0 (Cofre xs camino) = if (contieneTesoro xs) then 1 else 0
tesorosHasta n (Nada camino)     = tesorosHasta (n-1) camino
tesorosHasta n (Cofre xs camino) = if (contieneTesoro xs) then 1 + tesorosHasta (n-1) camino else tesorosHasta (n-1) camino

--Tipos Arbóreos


data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

treePrueba :: Tree Int
treePrueba = (NodeT 3 
                (NodeT 4 (NodeT 5 EmptyT EmptyT) EmptyT) 
                (NodeT 3 (NodeT 6 EmptyT (NodeT 6 EmptyT EmptyT)) EmptyT))

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT c i d) = c + sumarT i + sumarT d


sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT c i d) = 1 + sizeT i + sizeT d


mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT c i d) = (NodeT (c*2) (mapDobleT i) (mapDobleT d))


perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT a EmptyT        = False
perteneceT a (NodeT c i d) = (a == c) || (perteneceT a i) || (perteneceT a d)


aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT a EmptyT        = 0
aparicionesT a (NodeT c i d) = if (a == c) then 1 + (aparicionesT a i) + (aparicionesT a d) else (aparicionesT a i) + (aparicionesT a d)

leaves :: Tree a -> [a]
leaves EmptyT                  = []
leaves (NodeT c EmptyT EmptyT) = c : []
leaves (NodeT c i d)           = leaves i  ++ leaves d  



heightT :: Tree a -> Int
heightT (NodeT c EmptyT EmptyT) = 0
heightT (NodeT c i d)           = 1 + if (sizeT i > sizeT d) then heightT i else heightT d

mirrorT :: Tree a -> Tree a
mirrorT EmptyT         = EmptyT
mirrorT (NodeT c i d)  = (NodeT c (mirrorT d) (mirrorT i))

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT c i d) = toList i ++ [c] ++ toList d 

levelN :: Int -> Tree a -> [a]
levelN n EmptyT = []
levelN 0 (NodeT c i d) = c:[]
levelN n (NodeT c i d) = (levelN (n-1) i) ++ (levelN (n-1) d)

listPerLevel :: Tree a -> [[a]]
listPerLevel nodo = todosLosNiveles nodo (heightT nodo)

todosLosNiveles :: Tree a -> Int -> [[a]]
todosLosNiveles nodo 0 = (levelN 0 nodo):[]
todosLosNiveles nodo n = levelN n nodo : todosLosNiveles nodo (n-1)

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT        = []
ramaMasLarga (NodeT c i d) = c : if (sizeT i > sizeT d) then ramaMasLarga i else ramaMasLarga d


todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT        = []
todosLosCaminos (NodeT e i d) = (unir (e:[]) i) ++ (unir (e:[]) d) 

unir :: [a] -> Tree a -> [[a]]
unir xs EmptyT = []
unir xs (NodeT a EmptyT EmptyT)        = (a:xs):[]
unir xs (NodeT a i d) =  unir (a:xs) i ++ unir (a:xs) d


------------------------------------------------------------------








































