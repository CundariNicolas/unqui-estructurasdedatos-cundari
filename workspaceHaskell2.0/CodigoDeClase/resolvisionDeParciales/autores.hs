import Organizador
import Set2

-- Propósito: dadas dos personas y un organizador, denota el conjunto de 
-- aquellos programas en las que las personas programaron juntas.
-- O(C log C) donde C es la cantidad de programas totales
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
programasEnComun p1 p2 org = 
  -- O(log |Personas|)
  let programasDeP1 = (programasDe org p1) in 
    -- O(log |Personas|)
    let programasDeP2 = (programasDe org p2) in
      -- intersection O(|programasDeP1| log |programasDeP2|)
      intersection programasDeP1 programasDeP2

-- Propósito: denota verdadero si la persona indicada aparece como autor de 
-- todos los programas del organizador.
esUnGranHacker :: Organizador -> Persona -> Bool
esUnGranHacker org p = 
  (nroProgramasDePersona org p) == length (todosLosProgramas org)


google :: Organizador
google = 
  agregarPrograma (
  agregarPrograma ( 
  agregarPrograma nuevo 
   "Angular" (addS "Dev3" emptyS) )
   "Flutter" (addS "Dev2" emptyS) )
   "Polymer" (addS "Dev1" (addS "Dev2" emptyS)) 
  