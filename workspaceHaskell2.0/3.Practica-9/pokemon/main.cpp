#include <iostream>
#include "entrenador.h"

using namespace std;

string imprimirSiGanaATodos(bool i){
    string res;
    if(i){
        res = "Si.";
    }
    else{
        res ="No.";
    }
return res;
}

int main(){
    Pokemon charizard = consPokemon("fuego");
    Pokemon squartle = consPokemon("agua");
    Pokemon bulbasour = consPokemon("planta");

    Pokemon* pokemonesDeAsh = new Pokemon[1];
    pokemonesDeAsh[0] = charizard;

    Pokemon* pokemonesDeMisty = new Pokemon[3];
    pokemonesDeMisty[0] = charizard;
    pokemonesDeMisty[1] = squartle;
    pokemonesDeMisty[2] = bulbasour;

    Pokemon* pokemonesDeBrock = new Pokemon[2];
    pokemonesDeBrock[0] = squartle;
    pokemonesDeBrock[1] = bulbasour;

    Pokemon* perdedor = new Pokemon[2];
    perdedor[0] = bulbasour;
    perdedor[1] = bulbasour;

    Entrenador ash = consEntrenador("Ash", 1, pokemonesDeAsh);
    Entrenador misty = consEntrenador("Misty", 3, pokemonesDeMisty);
    Entrenador brock = consEntrenador("Brock", 2, pokemonesDeBrock);
    Entrenador perdido = consEntrenador("Perdedor", 2, perdedor);


    cout << "Nombre: " << nombreDeEntrenador(ash) << endl;
    cout << "Cantidad de Pokemones: " << cantidadDePokemon(ash) << endl;
    cout << "Cantidad de Tipo Fuego: " << cantidadDePokemonDe("fuego", ash) << endl;
    cout << "Cantidad de Tipo Agua: " << cantidadDePokemonDe("agua", ash) << endl;
    cout << "Cantidad de Tipo Planta: " << cantidadDePokemonDe("planta", ash) << endl;
    cout << "¿Le gana Ash a Perdido?: " << (imprimirSiGanaATodos(leGanaATodos(ash, perdido))) << endl;


    cout << "Nombre: " << nombreDeEntrenador(misty) << endl;
    cout << "Cantidad de Pokemones: " << cantidadDePokemon(misty) << endl;
    cout << "Cantidad de Tipo Fuego: " << cantidadDePokemonDe("fuego", misty) << endl;
    cout << "Cantidad de Tipo Agua: " << cantidadDePokemonDe("agua", misty) << endl;
    cout << "Cantidad de Tipo Planta: " << cantidadDePokemonDe("planta", misty) << endl;
    cout << "¿Le gana Misty a Ash?: " << (imprimirSiGanaATodos(leGanaATodos(misty, ash))) << endl;

    cout << "Nombre: " << nombreDeEntrenador(brock) << endl;
    cout << "Cantidad de Pokemones: " << cantidadDePokemon(brock) << endl;
    cout << "Cantidad de Tipo Fuego: " << cantidadDePokemonDe("fuego", brock) << endl;
    cout << "Cantidad de Tipo Agua: " << cantidadDePokemonDe("agua", brock) << endl;
    cout << "Cantidad de Tipo Planta: " << cantidadDePokemonDe("planta", brock) << endl;
    cout << "¿Le gana Brock a Ash?: " << (imprimirSiGanaATodos(leGanaATodos(brock, ash))) << endl;

    cout << "Nombre: " << nombreDeEntrenador(perdido) << endl;
    cout << "Cantidad de Pokemones: " << cantidadDePokemon(perdido) << endl;
    cout << "Cantidad de Tipo Fuego: " << cantidadDePokemonDe("fuego", perdido) << endl;
    cout << "Cantidad de Tipo Agua: " << cantidadDePokemonDe("agua", perdido) << endl;
    cout << "Cantidad de Tipo Planta: " << cantidadDePokemonDe("planta", perdido) << endl;
    cout << "¿Le gana Perdido a Brock?: " << (imprimirSiGanaATodos(leGanaATodos(perdido, brock))) << endl;
    



    

    return 0;
}