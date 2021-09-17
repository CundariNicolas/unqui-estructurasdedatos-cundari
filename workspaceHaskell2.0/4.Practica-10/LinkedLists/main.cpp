#include <iostream>
#include "linkedlist.h"

using namespace std;


LinkedList repetir(int n, int x){
    LinkedList xs = nil();
    while(n > 0){
        cons(x, xs);
        n--;
    }
    return xs;
}

void printLinkedList(LinkedList xs){
    initialize(xs);
    cout << "[ ";
    while(!finished(xs)){
        cout << current(xs) << " ";
        next(xs);
    }
    cout << "]" << endl;

}

int sumatoria(LinkedList xs){
    int res = 0;
    initialize(xs);
    while(!finished(xs)){
        res = res + current(xs);
        next(xs);
    }
    return res;

}

void sucesores(LinkedList xs){
    initialize(xs);
    while(!finished(xs)){
        setCurrent(current(xs)+1, xs);
        next(xs);
    }
}

int apariciones(int x, LinkedList xs){
    int ap = 0;
    initialize(xs);
    while(!finished(xs)){
        if(current(xs) == x){
            ap++;
        }
        next(xs);
    }
    return ap;
}

int minimo(LinkedList xs){
    int min;
    initialize(xs);
    min = current(xs);
    while(!finished(xs)){
        if(current(xs) < min){
            min = current(xs);
        }
        next(xs);
    }
    return min;
}


LinkedList copy(LinkedList xs){
    LinkedList ys = nil();
    initialize(xs);
    while(!finished(xs)){
            cons(current(xs), ys);
            next(xs);
    }
    return ys;
}


void append(LinkedList xs, LinkedList ys){
    initialize(ys);
    while(!finished(ys)){
            cons(current(ys), xs);
            next(ys);
    }
    destroyL(ys);
}

int main(){
    


}