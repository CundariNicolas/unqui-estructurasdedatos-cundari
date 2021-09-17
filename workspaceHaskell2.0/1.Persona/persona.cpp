#include <string>
#include "persona.h"
struct PersonaSt{
    std::string nombre;
}; 


int sumar (int a, int b) {
    return a + b;
}

Persona mkPersona(std::string nombre){
    Persona result = new PersonaSt;
    result->nombre = nombre;
    return result;
}


std::string nombre(Persona persona){
    return persona->nombre;
}

void destroyPersona(Persona persona){
    delete persona;
}