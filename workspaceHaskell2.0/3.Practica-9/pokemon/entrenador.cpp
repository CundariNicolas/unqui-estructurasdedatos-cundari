#include <iostream>
#include "entrenador.h"

struct EntrenadorSt{
    string nombre;
    Pokemon* pokemones;
    int cantidad;
};

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemones){
    EntrenadorSt* p = new EntrenadorSt;
    p->nombre = nombre;
    p->cantidad = cantidad;
    p->pokemones = pokemones;

    return p;

}


string nombreDeEntrenador(Entrenador e){
    return e->nombre;
}

int cantidadDePokemon(Entrenador e){
    return e->cantidad;
}

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e){
    int res = 0;
    for(int i = 0; i < e->cantidad ; i++){
        if(tipoDePokemon(e->pokemones[i]) == tipo){
            res++;
        }
    }
    

    return res;
}

Pokemon pokemonNro(int i, Entrenador e){
    return e->pokemones[i];
}

// Auxiliar
bool alMenosUno(Pokemon* pks, int cant, Pokemon p){
    bool res = false;
    for(int i = 0; i < cant ; i++){
        res = res || superaA(pks[i], p);
    }
    return res;
}

bool leGanaATodos(Entrenador e1, Entrenador e2){
    bool res = true;
    for(int i = 0 ; i < e2->cantidad; i++){
        if(alMenosUno(e1->pokemones, e1->cantidad , e2->pokemones[i])){
            res = res && true;
        }
        else{
            res = false;
        }
    }
    return res;
}