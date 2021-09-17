#include "persona.h"


struct PersonaSt{
    string nombre;
    int edad;
};


Persona consPersona(string nombre, int edad){
    PersonaSt* p = new PersonaSt;
    p->nombre = nombre;
    p->edad = edad;
    return p;

}

string nombre(Persona p){
    return p->nombre;
}

int edad(Persona e){
    return e->edad;
}

void crecer(Persona p){
    p->edad++;
}

void cambioDeNombre(string nombre, Persona p){
    p->nombre = nombre;
}

bool esMayorQueLaOtra(Persona p1, Persona p2){
    return (p1->edad > p2->edad);
}

Persona laQueEsMayor(Persona p1, Persona p2){
    if(esMayorQueLaOtra(p1, p2)){
        return p1;
    }
    else{
        return p2;
    }
}

void imprimirPersona(Persona p){
    cout << "Persona { " << endl;
    cout << "   Nombre: " << p->nombre << endl;
    cout << "   Edad: " << p->edad << endl;
    cout << "}" << endl; 
}