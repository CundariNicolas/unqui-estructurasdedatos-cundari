#include <iostream>
#include "arbolesBinarios.h"
#include "arraylist.h"

using namespace std;


int sumarT(Tree t){
    return (rootT(t)) + sumarT(left(t)) + sumarT(right(t));
}

int sizeT(Tree t){
    int res = 0;
    if(!isEmptyT(t)){
        res = 1 + sizeT(left(t)) + sizeT(right(t));
    }
    return res;
}


bool perteneceT(int e, Tree t){
    return (e == rootT(t)) || perteneceT(e, left(t)) || perteneceT(e, right(t));
}


int aparicionesT(int e, Tree t){
    if(!isEmptyT(t)){
        if(e == rootT(t)){
            return 1 + aparicionesT(e, left(t)) + aparicionesT(e, right(t));
        }
        else {
            return aparicionesT(e, left(t)) + aparicionesT(e, right(t));
        }
    }
    else{
        return 0;
    }
}


int heightT(Tree t){
    if(!isEmptyT(t)){
        if(sizeT(left(t)) > sizeT(right(t))){
            return 1 + heightT(left(t));
        }
        else{
            return 1+ heightT(right(t));
        }
    }
    else{
        return 0;
    }
}


ArrayList toList(Tree t){
    ArrayList arr = newArrayListWith(sizeT(t));
    if(!isEmptyT(t)){
    }
}


int main(){

}