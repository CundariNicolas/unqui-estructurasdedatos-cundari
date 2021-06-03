type Coordenada = (Int,Int)
data Direccion = Norte
                |Este
                |Sur
                |Oeste

desplazar :: Coordenada -> Direccion -> Coordenada
desplazar par Norte = (fst par + 1, snd par)
desplazar par Este  = (fst par, snd par + 1)
desplazar par Sur   = (fst par - 1, snd par)
desplazar par Oeste = (fst par, snd par - 1)

