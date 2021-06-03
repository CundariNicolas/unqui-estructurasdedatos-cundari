-- Ejercicio 2


nuevaA  :: Alcancia           
ponerA  :: Int -> Alcancia -> Alcancia
contarA :: Int -> Alcancia -> Int


vaquita :: [Alcancia] -> Alcancia
vaquita []     = nuevaA
vaquita (x:xs) = agregarTodasM x (vaquita xs)



agregarTodasM :: Alcancia -> Alcancia -> Alcancia
agregarTodasM alc vaq = ponerVarias 1 (contarA 1 alc) (ponerVarias 2 (contarA 2 alc) (ponerVarias 5 (contarA 5 alc) vaq))



ponerVarias :: Int -> Int -> Alcancia -> Alcancia
ponerVarias mon 0    alc = ponerA mon alc
ponerVarias mon cant alc = ponerVarias mon (cant-1) (ponerA mon alc) 


                                   


