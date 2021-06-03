porcionesPorPizza :: Integer
porcionesPorPizza = 8
{- Define los valores conocidos de 
el problema
-}
gramosDeHarinaPorPizza :: Integer
gramosDeHarinaPorPizza = 250

porcionesTotales :: Integer -> Integer -> Integer
porcionesTotales invitados porcionesPorInvitado = invitados * porcionesPorInvitado

pizzasTotales :: Integer -> Integer -> Integer
pizzasTotales invitados porcionesPorInvitado = 
    div (porcionesTotales invitados porcionesPorInvitado)
    porcionesPorPizza

-- Proposito : Calcula la cantidad
-- de gramos de harina en total
-- para n invitiados, suponiendo que cada 
-- invitado come m porciones de pizza
gramosDeHarinaTotales :: Integer -> Integer -> Integer
gramosDeHarinaTotales n m =
    pizzasTotales n m * gramosDeHarinaPorPizza