#include <iostream>
#include "queue.h"

struct NodoQ{
    int elem; // valor del nodo
    NodoQ* siguiente; // puntero al siguiente nodo
};

struct QueueSt{
    int cantidad; // cantidad de elementos
    NodoQ* primero; // puntero al primer nodo
    NodoQ* ultimo; // puntero al ultimo nodo
};

Queue emptyQ(){
    QueueSt* q = new QueueSt;
    q->cantidad = 0;
    q->primero = NULL;
    q->ultimo = NULL;
}

bool isEmptyQ(Queue q){
    return q->cantidad > 0;
}

int firstQ(Queue q){
    return q->primero->elem;
}

void enqueue(int x, Queue q){
    NodoQ* anterior = q->ultimo;
    NodoQ* nuevo = new NodoQ;
    nuevo->elem = x;
    nuevo->siguiente = NULL;
    q->ultimo = nuevo;
    anterior->siguiente = nuevo;
    q->cantidad++;
}

void dequeue(Queue q){
    if(!isEmptyQ(q)){
        NodoQ* exP = q->primero;
        q->primero = q->primero->siguiente;
        q->cantidad--;
        delete exP;
    }
}

int lengthQ(Queue q){
    return q->cantidad;
}

void mergeQ(Queue q1, Queue q2){
    q1->ultimo->siguiente = q2->primero;
    q1->cantidad = q1->cantidad + q2->cantidad;
    delete q2;

}

void destroyQ(Queue q){
    while(!isEmptyQ(q))
    {
        dequeue(q);
    }
    delete q;
    
}

