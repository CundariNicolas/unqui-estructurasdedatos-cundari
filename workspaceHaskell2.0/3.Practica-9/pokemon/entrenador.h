#include <iostream>
#include "pokemon.h"

using namespace std;

struct EntrenadorSt;

typedef EntrenadorSt* Entrenador;

Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemones);

string nombreDeEntrenador(Entrenador e);

int cantidadDePokemon(Entrenador e);

int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e);

Pokemon pokemonNro(int i, Entrenador e);

bool leGanaATodos(Entrenador e1, Entrenador e2);
