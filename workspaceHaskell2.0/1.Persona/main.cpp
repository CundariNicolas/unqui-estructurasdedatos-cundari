#include <iostream> 
#include <string>
#include "persona.h"


int main(){
    Persona german = mkPersona("Nicolas");
    std::string n = nombre(german);
    destroyPersona(german);

    std::cout << "Hola " << n << std::endl;
    return 0;
}


