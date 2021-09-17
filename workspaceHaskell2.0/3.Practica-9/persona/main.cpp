#include <iostream>
#include "persona.h"

using namespace std;

int main(){
    Persona nico = consPersona("Nicolas", 26);
    Persona jose = consPersona("Jose", 57);
    imprimirPersona(nico);


    crecer(nico);

    imprimirPersona(nico);
    cambioDeNombre("Roberto", nico);

    imprimirPersona(nico);

    imprimirPersona(laQueEsMayor(nico, jose));



    return 0;
}