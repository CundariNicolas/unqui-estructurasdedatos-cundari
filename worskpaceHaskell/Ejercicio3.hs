--Ejercicio 3

data Museo = M (Map DNI Int)    -- A cada persona en el museo le asocia su edad.
               (MultiSet Int)   -- Multiconjunto de las edades de las personas
                                -- del museo, consideradas con su repetición.

type DNI = Int

nuevoM :: Museo 
nuevoM = M  (emptyM) 
            (emptyMS)

entrarM :: DNI -> Int -> Museo -> Museo
entrarM dni e (M map mul) = M (assocM dni e map) (addMS e mul)

salirM :: DNI -> Museo -> Museo
salirM  dni (M map mul) = M (deleteM dni map) mul

cuantosDe :: Int -> Museo -> Int
cuantosDe n (M map mul) = ocurrencesMS n mul
