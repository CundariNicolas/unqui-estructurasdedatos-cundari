{-
en gobstones sería algo así
function minimo(x,y){
    return ( choose x when x<y
                    y otherwise)
}

-}

minimo :: Integer -> Integer -> Integer
minimo valor1 valor2 = 
    if valor1 < valor2 
        then valor1 
        else valor2

minimo3 :: Integer -> Integer -> Integer -> Integer
minimo3 x y z =
    if minimo x y < z
        then minimo x y
        else z


maximo :: Integer -> Integer -> Integer
maximo valor1 valor2 = 
    if valor1 > valor2 
        then valor1 
        else valor2

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z =
    if maximo x y < z
        then maximo x y
        else z





--Proposito: devuelve el valor medio entre x y z
mediano3 :: Integer -> Integer -> Integer -> Integer
mediano3 x y z =
     if (x > y && x < z) || (x < y && x > z)
         then x
         else if (y > x && y < z) || (y < x && y > z) 
                then y
                else z
