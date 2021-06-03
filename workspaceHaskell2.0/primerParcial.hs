---PARCIAL---

data Sanguche   = Pan Relleno deriving Show
data Relleno    = Feta TipoDeFeta Relleno | Aire deriving Show
data TipoDeFeta = Queso | Jamon | Mortadela | Salame deriving (Show, Eq)

sanguche1 = Pan (Feta Jamon (Feta Jamon (Feta Jamon (Feta Queso (Feta Queso (Feta Queso (Feta Jamon (Feta Jamon (Feta Queso (Feta Queso (Feta Jamon Aire)))))))))))

--Resolver las siguientes funciones utilizando recursión estructural:
--Propósito: Dado un sanguche, indica si el relleno es solo de aire.
rellenoDeAire :: Sanguche -> Bool
rellenoDeAire (Pan r) = esAire r

esAire :: Relleno -> Bool
esAire Aire = True
esAire _    = False




--Propósito: Dado un sanguche indica si solo tiene fetas de jamon.
esTortitaDeJamon :: Sanguche -> Bool
esTortitaDeJamon (Pan r) = soloJamon r



soloJamon :: Relleno -> Bool
soloJamon Aire          = True
soloJamon (Feta t r)    = esJamon t && soloJamon r

esJamon :: TipoDeFeta -> Bool
esJamon Jamon = True
esJamon _     = False

--Propósito: Dados un número n y un tipo de feta, agrega n fetas de ese tipo, al principio del relleno del sanguche dado.
mandaleNDe :: Int -> TipoDeFeta -> Sanguche -> Sanguche
mandaleNDe 1 t s = agregarFeta t s
mandaleNDe n t s = agregarFeta t (mandaleNDe (n-1) t s)

agregarFeta:: TipoDeFeta -> Sanguche -> Sanguche
agregarFeta t (Pan r) = Pan (agregarAlRelleno t r)

agregarAlRelleno :: TipoDeFeta -> Relleno -> Relleno
agregarAlRelleno t Aire       = Feta t (Aire)
agregarAlRelleno t feta       = Feta t feta



--Propósito: Quita todo el queso del relleno al sanguche dado.
peroSinQueso :: Sanguche -> Sanguche
peroSinQueso (Pan r) = Pan (sacarQueso r)

sacarQueso :: Relleno -> Relleno
sacarQueso Aire         = Aire
sacarQueso (Feta t r)   = if (esQueso t) then sacarQueso r else Feta t (sacarQueso r)

esQueso :: TipoDeFeta -> Bool
esQueso Queso = True
esQueso _     = False





--Propósito: Devuelve una lista de fetas del relleno del sanguche, junto con su cantidad de apariciones.
ordenadosPorCantidad :: Sanguche -> [(TipoDeFeta, Int)]
ordenadosPorCantidad (Pan r) = todasLasFetas r

todasLasFetas :: Relleno -> [(TipoDeFeta, Int)]
todasLasFetas Aire          = []
todasLasFetas (Feta t r)    = sinParRepetido ((t, cantidadDeFetas t r) : todasLasFetas r)

sinParRepetido :: [(TipoDeFeta, Int)] -> [(TipoDeFeta, Int)]
sinParRepetido (x:[])     = x:[]
sinParRepetido (x:xs)  = quedarseConMayor x (sinParRepetido xs)

quedarseConMayor :: (TipoDeFeta, Int) -> [(TipoDeFeta, Int)] -> [(TipoDeFeta, Int)]
quedarseConMayor t []           = t:[]
quedarseConMayor t (x:xs)  = if (esIgualT t x && esMayor t x) then t : quedarseConMayor t xs else quedarseConMayor t xs

esIgualT :: (TipoDeFeta, Int) -> (TipoDeFeta, Int) -> Bool
esIgualT (t, e) (t1, e2) = (t == t1)

esMayor :: (TipoDeFeta, Int) -> (TipoDeFeta, Int) -> Bool
esMayor (t, e) (t1, e2) = (e > e2)


cantidadDeFetas :: TipoDeFeta -> Relleno -> Int
cantidadDeFetas t Aire = 0
cantidadDeFetas t (Feta t1 r) = if (t==t1) then 1 + cantidadDeFetas t r else cantidadDeFetas t r







-----------------------------------------------------------------------------------------


type Llave = Int

data Dir = Izq | Der deriving Show

data Cofre = Cofre [Llave] Int


data Laberinto = Salida | Celda Cofre | Bifurcacion Laberinto Laberinto


laberinto1 = Bifurcacion (Bifurcacion (Celda (cofre1))(Celda cofre2))(Bifurcacion (Celda cofre3)(Bifurcacion (Salida)(Bifurcacion (Salida) (Celda cofre1))))
cofre1 = Cofre [1] 20
cofre2 = Cofre [2] 40
cofre3 = Cofre [3] 60




cantidadDeSalidas :: Laberinto -> Int
cantidadDeSalidas Salida = 1
cantidadDeSalidas (Celda c) = 0
cantidadDeSalidas (Bifurcacion c1 c2) = cantidadDeSalidas c1 + cantidadDeSalidas c2

---

queLlavesDeboTener :: Laberinto -> [Llave]
queLlavesDeboTener Salida = []
queLlavesDeboTener (Celda c) = llavesDe c 
queLlavesDeboTener (Bifurcacion c1 c2) = sinRepetidos (queLlavesDeboTener c1 ++ queLlavesDeboTener c2)

llavesDe :: Cofre -> [Llave]
llavesDe (Cofre l o) = l

sinRepetidos :: [Llave] -> [Llave]
sinRepetidos [] = []
sinRepetidos (x:xs) = if (estaEn x xs) then sinRepetidos xs else x:sinRepetidos xs

estaEn :: Llave -> [Llave] -> Bool
estaEn l []    = False
estaEn l (x:xs)= (l == x) || estaEn l xs



----

cantidadDeOroCon :: [Llave] -> Laberinto -> Int
cantidadDeOroCon [] l     = 0
cantidadDeOroCon (x:xs) l = cofreDeLlave x l + cantidadDeOroCon xs l

cofreDeLlave :: Llave -> Laberinto -> Int
cofreDeLlave l Salida    = 0
cofreDeLlave l (Celda c) = if (esLlaveDeEseCofre l c) then oroDeCofre c else 0
cofreDeLlave l (Bifurcacion c1 c2) = cofreDeLlave l c1 + cofreDeLlave l c2

esLlaveDeEseCofre :: Llave -> Cofre -> Bool
esLlaveDeEseCofre l (Cofre xs o) = esLlave l xs

esLlave :: Llave -> [Llave] -> Bool
esLlave l [] = False
esLlave l (x:xs) = l == x || esLlave l xs

oroDeCofre :: Cofre -> Int
oroDeCofre (Cofre l o) = o


------------


haySalidaPor :: [Dir] -> Laberinto -> Bool
haySalidaPor [] Salida = True
haySalidaPor []  _     = False
haySalidaPor (x:xs) (Bifurcacion c1 c2)   = case x of 
                                            Izq -> haySalidaPor xs c1
                                            Der -> haySalidaPor xs c2



--------------------------------------------------------------
--Precondición: Existe al menos una salida
--Propósito: Indica el camino a la salida más cercana.

salidaMasCercana :: Laberinto -> [Dir]
salidaMasCercana l = caminoMasCorto (caminosQueTienenSalida (todosLosCaminos l) l)

todosLosCaminos :: Laberinto -> [[Dir]]
todosLosCaminos Salida              = []
todosLosCaminos (Celda c)           = []
todosLosCaminos (Bifurcacion c1 c2) = agregarATodos Izq (todosLosCaminos c1) ++ agregarATodos Der (todosLosCaminos c2)


agregarATodos :: Dir -> [[Dir]] -> [[Dir]]
agregarATodos d [] = [d:[]]
agregarATodos d (x:[])     = [[d] ++ x]
agregarATodos d (x:xs) = (d:x) : agregarATodos d xs


caminosQueTienenSalida :: [[Dir]] -> Laberinto -> [[Dir]]
caminosQueTienenSalida [] l       = []
caminosQueTienenSalida (x:xs) l   = if (haySalidaPor x l) then x : caminosQueTienenSalida xs l else caminosQueTienenSalida xs l


caminoMasCorto :: [[Dir]] -> [Dir]
caminoMasCorto (x:[]) = x
caminoMasCorto (x:xs) = masCorto x (caminoMasCorto xs)

masCorto :: [Dir] -> [Dir] -> [Dir]
masCorto dir1 dir2 = if (length dir1 < length dir2) then dir1 else dir2