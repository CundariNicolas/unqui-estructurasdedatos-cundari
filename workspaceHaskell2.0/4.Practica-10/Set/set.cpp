#include <iostream>
#include "set.h"

using namespace std;

struct NodoS{
    int elem;
    NodoS* siguiente;
};

struct SetSt{
    int cantidad;
    NodoS* primero;
};


Set emptyS(){
    SetSt* set = new SetSt;
    set->cantidad = 0;
    set->primero = NULL;
}

bool isEmptyS(Set s){
    return (s->primero == NULL);
}

bool belongsS(int x, Set s){
    bool res = false;
    NodoS* actual = s->primero;
    while((actual != NULL ) && (!res)){
        if(actual->elem == x){
            res = true;
        }
        actual = actual->siguiente;
    }
    return res;
}

void addS(int x, Set s){
    if(!belongsS(x, s)){
        NodoS* nuevo = new NodoS;
        nuevo->elem = x;
        nuevo->siguiente = s->primero;
        s->cantidad++;
        s->primero = nuevo;
    }
}

void removeS(int x, Set s){
    NodoS* anterior = NULL;
    NodoS* actual = s->primero;
    NodoS* sigAlActual = actual->siguiente;
    if(belongsS(x, s)){
        while(!(actual->elem == x)){
            anterior = actual;
            actual = sigAlActual;
            sigAlActual = sigAlActual->siguiente;
        }
        anterior->siguiente = sigAlActual;
        delete actual;
        s->cantidad--;
    }
}

int sizeS(Set s){
    return s->cantidad;

}

LinkedList setToList(Set s){
    LinkedList xs = nil();
    NodoS* actual = s->primero;
    while(actual != NULL){
        cons(s->primero->elem, xs);
        actual = actual->siguiente;
    }
    return xs;

}

void destroyS(Set s){
    NodoS* borrar = s->primero;
    while(!isEmptyS(s)){
        s->primero = s->primero->siguiente;
        delete borrar;
        s->cantidad--;
    }
    delete s;

}