data Simpson = Homero
             | Marge
             | Bart
             | Lisa
             | Maggie
             deriving Show

    {-Es una función que devuelve una edad como choose
    pasandole como argumento un SIMPSON
    -}
edad :: Simpson -> Integer
edad Homero = 36
edad Marge  = 34
edad Bart   = 10
edad Lisa   = 8
edad Maggie = 1

-- ES UNA FUNCIÓN PARCIAL, PUEDE FALLAR, POR EJEMPLO SI
-- QUIERO VER QUIENB ES LA MADRE DE MARGE
-- PRECONDICIÓN: EL SIMPSON DEBE SER BART, LISA , O MAGGIE.
madre :: Simpson -> Simpson
madre Bart = Marge
madre Lisa = Marge
madre Maggie= Marge