-- Ejercicio 1

type Nombre  = String
type Fecha   = Int
data Sistema = Archivo Nombre Fecha | Carpeta Nombre Sistema Sistema
type Ruta    = [Nombre]


filtrarPorFecha :: [Ruta] -> Fecha -> Sistema -> [Ruta]
filtrarPorFecha [] f s      = [] 
filtrarPorFecha (r:rts) f s = (traerRutaPorFecha r f s) : filtrarPorFecha rts f s


traerRutaPorFecha :: Ruta -> Fecha -> Sistema -> Ruta
traerRutaPorFecha [] f s     =  []
traerRutaPorFecha (x:xs) f s =  if hayArchivo x f s 
                                then x: traerRutaPorFecha xs f s 
                                else traerRutaPorFecha xs f s 


hayArchivo :: [Nombre] -> Fecha -> Sistema -> Bool
hayArchivo (x:[]) f s                 = esValido x f s 
hayArchivo (x:xs) f (Carpeta n si sd) = if (x == n) then hayArchivo xs f si || hayArchivo xs f sd else False

esValido :: Nombre -> Fecha -> Sistema -> Bool
esValido n f (Archivo nom fec) = (n == nom && f == fec)
esValido n f _                 = False

