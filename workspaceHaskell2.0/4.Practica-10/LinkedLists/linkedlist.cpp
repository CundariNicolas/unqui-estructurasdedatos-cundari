#include <iostream>
#include "linkedlist.h"

using namespace std;

struct NodoL{
    int elem; // valor del nodo
    NodoL* siguiente; // puntero al siguiente nodo
};

struct LinkedListSt{
    int cantidad; // cantidad de elementos
    NodoL* primero; // puntero al primer nodo
    NodoL* actual; // puntero al nodo actual
};

LinkedList nil(){
    LinkedListSt* ls = new LinkedListSt;
    ls->cantidad = 0;
    ls->primero = NULL;
    ls->actual = NULL;
    return ls;
}

bool isEmpty(LinkedList xs){
    return xs->primero == NULL;
}

int head(LinkedList xs){
    return xs->primero->elem;
}

void cons(int x, LinkedList xs){
    NodoL* nuevoNodo = new NodoL;
    nuevoNodo->elem = x;
    nuevoNodo->siguiente = xs->primero;
    xs->primero = nuevoNodo;
    xs->cantidad++;
}


void tail(LinkedList xs){
    NodoL* exPrimero = xs->primero;
    xs->primero = xs->primero->siguiente;
    delete exPrimero;
    xs->cantidad--;
}

int length(LinkedList xs){
    return xs->cantidad;
}

void snoc(int x, LinkedList xs){
    NodoL* nuevo = new NodoL;
    NodoL* ultimo = xs->actual;
    nuevo->elem = x;
    nuevo->siguiente = NULL;
    if(ultimo != NULL){
        while(ultimo->siguiente != NULL){
        ultimo = ultimo->siguiente;
        }
        ultimo->siguiente = nuevo;
    }
    else{
        xs->primero = nuevo;
    }
}


void initialize(LinkedList xs){
    xs->actual = xs->primero;
}

int current(LinkedList xs){
    return xs->actual->elem;
}

void setCurrent(int x, LinkedList xs){
    xs->actual->elem = x;
}

void next(LinkedList xs){
    xs->actual = xs->actual->siguiente;
}

bool finished(LinkedList xs){
    return xs->actual == NULL;
}

void destroyL(LinkedList xs){
    while(!isEmpty(xs)){
        tail(xs);
    }
    delete xs;
}



