
--3 Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:
data Color = Azul | Rojo deriving (Show, Eq)
data Celda = ConsCelda [Color] deriving Show


celdaVacia :: Celda
celdaVacia = ConsCelda []

nroBolitas :: Color -> Celda -> Int
nroBolitas c (ConsCelda []) = 0
nroBolitas c (ConsCelda (x:xs)) = if (c==x) then 1 + nroBolitas c (ConsCelda xs) else 0 + nroBolitas c (ConsCelda xs)

-- Dado un color y una celda, agrega una bolita de dicho color a la celda
poner :: Color -> Celda -> Celda
poner c (ConsCelda []) = ConsCelda (c:[])
poner c (ConsCelda xs) = ConsCelda (c:xs)


-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia
-- de Gobstones, esta función es total.
sacar :: Color -> Celda -> Celda
sacar c (ConsCelda []) = ConsCelda []
sacar c (ConsCelda (x:xs)) = ConsCelda (eliminar c (x:xs)) 

eliminar :: Color -> [Color] -> [Color]
eliminar c [] = []
eliminar c (x:xs) = if c==x then xs else x:(eliminar c xs)


--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN :: Int -> Color -> Celda -> Celda
ponerN n c (ConsCelda xs) = (ConsCelda (agregarNVeces n c xs))


agregarNVeces:: Int -> Color -> [Color] -> [Color]
agregarNVeces n c [] = (c:[])
agregarNVeces 1 c xs = (c:xs)
agregarNVeces n c xs = (c: agregarNVeces (n-1) c xs)







-- Representaremos una secuencia de celdas con bolitas de colores, donde existe un cabezal
-- ubicado en una determinada celda. La representación es la siguiente:
celda1= ConsCelda [Rojo, Azul, Rojo]
celda2= ConsCelda [Rojo, Rojo, Rojo]
celda3= ConsCelda [Azul,Azul,Azul]
celda4= ConsCelda [Azul,Azul,Rojo,Rojo]
celda5= ConsCelda [Azul,Azul,Azul,Azul,Rojo]
celda6= ConsCelda [Azul,Azul,Azul,Rojo,Rojo]

secuencia1= Sec [celda1,celda2,celda3] celda4 [celda5,celda6]

secuencia2= Sec [celda1,celda2] celda3 [celda4,celda5,celda6]

secuencia3 = Sec [] celda3 [celda4,celda5]

secuencia4 = Sec [celda1] celda2 [celda3,celda4,celda5,celda6]

data SecuenciaDeCeldas = Sec [Celda] Celda [Celda] deriving Show

-- En dicha representación, el primer parámetro del constructor representa la lista de celdas
-- anterior a la actual; el segundo parámetro indica la celda donde el cabezal está ubicado; y el
-- tercero es la lista de celdas siguientes a la actual. En otras palabras, el orden está dado por
-- la aparición de las celdas desde la primera lista hasta la segunda lista, teniendo en el medio
-- la celda que es segundo parámetro del constructor.
-- Para representar movimientos del cabezal dentro de dicha secuencia, usaremos el siguiente
-- enumerativo, que indica las direcciones a las que nos podremos mover (izquierda o derecha):

data Dir = Izq | Der deriving Show 

--Dicho esto, definir las siguientes funciones que operan sobre secuencias de celdas:

--1) Dada una secuencia de celdas, denota la lista de celdas que dicha secuencia representa,
--respetando el orden de la secuencia.
celdas :: SecuenciaDeCeldas -> [Celda]
celdas (Sec i a d) = i ++ [a] ++ d 

--2) 
--Dada una secuencia de celdas, indica si el cabezal está ubicado en el extremo izquierdo

enElOrigen :: SecuenciaDeCeldas -> Bool
enElOrigen (Sec [] i d) = True
enElOrigen (Sec d _ _) = False



-- Dado un número n, y una lista de celdas cs, produce una secuencia que sigue el orden
-- de la lista cs, y en la que la celda número n de la lista tendrá ubicado el cabezal.
-- Precondición: el número n es un índice válido de la lista y la lista cs no está vacía.

nuevaSecuencia :: Int -> [Celda] -> SecuenciaDeCeldas
nuevaSecuencia n [] = error " Necesita una lista no vacía "
nuevaSecuencia n cs = Sec (celdasAnteriores n cs) (celdaN n cs) (celdasPosteriores n cs)


celdasAnteriores :: Int -> [Celda] -> [Celda]
celdasAnteriores 1 (c:cs) = []
celdasAnteriores n (c:cs) = [c] ++ celdasAnteriores (n-1) cs

celdaN :: Int -> [Celda] -> Celda
celdaN 1 (c:cs) = c
celdaN n (c:cs) = celdaN (n-1) cs

celdasPosteriores :: Int -> [Celda] -> [Celda] 
celdasPosteriores 1 (c:cs) = cs
celdasPosteriores n (c:cs) = celdasPosteriores (n-1) cs




-- Dada una dirección y una secuencia de celdas, avanza el cabezal hacia la celda lindante
-- en la dirección dada. Nota: a diferencia de Gobstones esta operación es total.

mover :: Dir -> SecuenciaDeCeldas -> SecuenciaDeCeldas
mover Der (Sec [] a (d:ds)) = (Sec [a] d (ds))
mover Izq (Sec [] a (d:ds)) = (Sec [] a (d:ds))

mover Der (Sec (i:is) a []) = (Sec (i:is) a [])
mover Izq (Sec (i:is) a []) = (Sec (i:(sinUltimo is)) (elUltimo is) [a])

mover Der (Sec (i:is) a (d:ds)) = (Sec ((i:is) ++ [a]) d (ds))
mover Izq (Sec (i:is) a (d:ds)) = (Sec (i:(sinUltimo is)) (elUltimo is) (a:d:ds))

-----Auxiliares

sinUltimo:: [Celda] -> [Celda]
sinUltimo (c:[]) = []
sinUltimo (c:cs) = c:(sinUltimo cs)


elUltimo :: [Celda] -> Celda
elUltimo (c:[]) = c
elUltimo (c:cs) = (elUltimo cs)

-- Dado un número n, una dirección y una secuencia de celdas, mueve el cabezal n veces
-- hacia dicha dirección. Nota: a diferencia de Gobstones esta operación es total.


moverN :: Int -> Dir -> SecuenciaDeCeldas -> SecuenciaDeCeldas 
moverN 1 dir (Sec (i:is) a (d:ds)) = mover dir (Sec (i:is) a (d:ds))
moverN n dir (Sec (i:is) a (d:ds)) = moverN (n-1) dir (mover dir (Sec (i:is) a (d:ds)))


-- Dada una secuencia de celdas, ubica el cabezal en el extremo izquierdo. Nota: sea astuto
-- y piense de forma denotacional (no operacional).

irAlOrigen:: SecuenciaDeCeldas -> SecuenciaDeCeldas
irAlOrigen (Sec (i:is) a (d:ds)) = (Sec [] i (is ++ a:d:ds)) 


-- Dada una secuencia de celdas, indica la cantidad de bolitas rojas y azules. Nota: sea
-- astuto y piense de forma denotacional (no operacional).

totalDeBolitas :: SecuenciaDeCeldas -> Int
totalDeBolitas (Sec [] a s) = (nroBolitas Rojo a) + (nroBolitas Azul a) + bolitasDe s
totalDeBolitas (Sec q a [])  = bolitasDe q + (nroBolitas Rojo a) + (nroBolitas Azul a)
totalDeBolitas (Sec q a s) = bolitasDe q + (nroBolitas Rojo a) + (nroBolitas Azul a) + bolitasDe s

--Auxuliar

bolitasDe :: [Celda] -> Int
bolitasDe [] = 0
bolitasDe (x:xs) = (nroBolitas Rojo x) + (nroBolitas Azul x) + (bolitasDe xs)


----------------

-- Dada una secuencia de celdas, vacía las celdas de la misma, manteniendo el cabezal en
-- la posición actual. Nota: sea astuto y piense de forma denotacional (no operacional).


vaciar :: SecuenciaDeCeldas -> SecuenciaDeCeldas
vaciar (Sec [] a []) = (Sec [] a [])
vaciar (Sec [] a (d:ds)) = (Sec [] a [])
vaciar (Sec (i:is) a []) = (Sec [] a [])
vaciar (Sec (i:is) a (d:ds)) = (Sec [] a [])