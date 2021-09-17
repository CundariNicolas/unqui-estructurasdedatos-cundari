#include <iostream>
#include "arraylist.h"

using namespace std;

struct ArrayListSt{
    int cantidad; //cantidad de elementos
    int* elementos; //array de elementos
    int capacidad; // tamaño del array
};

ArrayList newArrayList(){
    ArrayListSt* arr = new ArrayListSt;
    arr->cantidad = 0;
    arr->elementos = new int[16];
    arr->capacidad = 16;

    return arr;
}

ArrayList newArrayListWith(int capacidad){
    ArrayListSt* arr = new ArrayListSt;
    arr->cantidad = 0;
    arr->elementos = new int[capacidad];
    arr->capacidad = capacidad;

    return arr;
}

int lengthAL(ArrayList xs){
    return xs->cantidad;
}

int get(int i, ArrayList xs){
    return xs->elementos[i];
}

void set(int i, int x, ArrayList xs){
    xs->elementos[i] = x;
}

void resize(int capacidad, ArrayList xs){
    // guarda el array anterior para poder copiarlo
    int* arrayAntiguo = xs->elementos;
    int cantidadAntiguo = xs->cantidad;
    xs->cantidad = 0;

    // si el array se achica considera si tiene que borrar elementos
    // si el array está lleno sino simplemente copia lo que había
    // antes en un array con más capacidad

    if(capacidad < xs->capacidad){
        xs->elementos = new int[capacidad];
        // si la cantidad de antes es mayor a la nueva capacidad
        // sólo copia hasta el máximo de la capacidad
        // si el array es más grande de la cantidad de elementos
        // sólo copia hasta la cantidad de antes
                        if(cantidadAntiguo > capacidad){
                            for(int i = 0; i < capacidad; i++){
                                xs->elementos[i] = arrayAntiguo[i];
                                xs->cantidad++;
                            }
                        
                        }
                        else{
                            for(int i = 0; i < cantidadAntiguo ; i++){
                                xs->elementos[i] = arrayAntiguo[i];
                                xs->cantidad = cantidadAntiguo;
                            }
                        }

    }
    else{
        xs->elementos = new int[capacidad];
        for(int i = 0; i < xs->cantidad; i++){
            xs->elementos[i] = arrayAntiguo[i];
        }
        
    }
    xs->capacidad = capacidad;
    delete arrayAntiguo;
}

void add(int x, ArrayList xs){
    if(xs->cantidad < xs->capacidad){
        xs->elementos[xs->cantidad] = x;
        xs->cantidad++;
    }
}

void remove(ArrayList xs){
    int nuevaCantidad = (xs->cantidad) - 1;
    int* antiguoArray = xs->elementos;
    if(xs->cantidad > 0){
        xs->elementos = new int[xs->capacidad];
        for(int i = 0; i < nuevaCantidad; i++){
            xs->elementos[i] = antiguoArray[i];
        }
        xs->cantidad--;

    }
    delete antiguoArray;
}