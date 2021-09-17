#include <iostream>
#include "arraylist.h"

using namespace std;

// funciones como usuario de arraylist

int sumatoria(ArrayList xs){
    int res = 0;
    for(int i = 0; i < lengthAL(xs); i++ ){
        res = res + get(i, xs);
    }
    return res;
}

void sucesores(ArrayList xs){
    for(int i = 0; i < lengthAL(xs); i++ ){
        set(i, (get(i, xs) + 1), xs);
    }
}

bool pertenece(int x, ArrayList xs){
    bool res = false;
    int acc = 0;
    while ( !res && acc < lengthAL(xs)){
        if(get(acc, xs) == x){
            res = true;
        }
        else{
            acc++;
        }
    }
    return res;

}


int apariciones(int x, ArrayList xs){
    int acc = 0;
    int cant = 0;
    while(acc < lengthAL(xs)){
        if(get(acc, xs) == x){
            cant++;
        }
        acc++;
    }
    return cant;
}


ArrayList append(ArrayList xs, ArrayList ys){
    ArrayList arr = newArrayListWith(lengthAL(xs) + lengthAL(ys));
    int acc = 0;

    while(acc < lengthAL(xs)){
        add(get(acc, xs), arr);
        acc++;
    }
    acc = 0;
    while(acc < lengthAL(ys)){
        add(get(acc, ys), arr);
        acc++;
    }

    return arr;
}

int main(){
    ArrayList arr1 = newArrayList();
    ArrayList arr2 = newArrayListWith(26);

    int a = 1;

    for (int i = 0; i < 26; i++)
    {
        add(a, arr2);
        a++;
    }
    
    for(int i = 0; i < 26; i++){
        cout << get(i, arr2) << " ";
    }
    cout << "]" << endl;;



    for(int i = 0; i < 20; i++){
        add(a, arr1);
        a++;
    }
    cout << "[ ";
    for(int i = 0; i < 20; i++){
        cout << get(i, arr1) << " ";
    }
    cout << "]" << endl;;


    remove(arr1);

    cout << "[ ";
    for(int i = 0; i < 16; i++){
        cout << get(i, arr1) << " ";
    }
    cout << "]" << endl;

    cout << lengthAL(arr1) << endl;

    resize(13, arr1);

    for(int i = 0; i < 20; i++){
        add(a, arr1);
        a++;
    }

     cout << "[ ";
    for(int i = 0; i < 20; i++){
        cout << get(i, arr1) << " ";
    }
    cout << "]" << endl;



    ArrayList arr3 = append(arr1, arr2);

    cout << "[ ";
    for(int i = 0; i < lengthAL(arr3); i++){
        cout << get(i, arr3) << " ";
    }
    cout << "]" << endl;


    sucesores(arr3);

    cout << "[ ";
    for(int i = 0; i < lengthAL(arr3); i++){
        cout << get(i, arr3) << " ";
    }
    cout << "]" << endl;

    cout << "Apariciones de 25: " << apariciones(27, arr3) << "." << endl; 









    return 0;
}