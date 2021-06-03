{- Proposito: Calcula el numero de segundos en total que hay
comprendidos en d dias, h horas , m minutos , y s segundos
-}

horasEnTotal :: Integer -> Integer -> Integer
horasEnTotal d h = 24 * d + h

minutosEnTotal :: Integer -> Integer -> Integer -> Integer
minutosEnTotal d h m = 
    horasEnTotal d h * 60 + m


segundosEnTotal :: Integer -> Integer -> Integer -> Integer -> Integer
segundosEnTotal d h m s =
    minutosEnTotal d h m * 60 + s


--segundosEnTotal d h m s = .........



-- Ejemplo
-- segundosEnTotal 0 0 1 0
-- -->60
-- segundosEnTotal 0 0 2 30
-- 150
