#include <iostream>
#include <algorithm>
using namespace std;

void printFromTo(char c1, char c2){
    for (int i = 0; c1 + i <= c2; i++){
        cout << c1 + i << ", ";
    }
    cout << endl;
};


int fc(int n){
    int x = 1;
    while(n > 0){
    x = x * n;
    n--;
    }

    return x;

};

int ft (int n, int m){
        if(n == m){
            return n;
        };
    return n + ft(n+1, m);
}


struct Par {
    int x;
    int y;
};

Par consPar(int x, int y){
    Par p;
    p.x = x;
    p.y = y;
    return p;
};


int fst (Par p){
    return p.x;
}

int snd (Par p){
    return p.y;
}


int maxDelPar (Par p){
    if (fst(p) > snd(p)){
        return fst(p);
    }
    else {
        return snd(p);
    }
}


Par swap(Par p){
    Par result;
    result.x = p.y;
    result.y = p.x;

    return result;
}


Par divisionYResto(int n, int m){
    Par result;
    result.x = n/m;
    result.y = n%m;

    return result;

}


void mostrarPar(Par p){
    std::cout << "(" << p.x << ", " << p.y << ")" << std::endl;
}


void printN(int n, string s){
    for(int i = n; i > 0; i--){
        std::cout << s << std::endl;
    };
}

void cuentaRegresiva (int n) {
    for (int i = n; i > 0; i--){
        cout << i << endl;
    }
}


void desdeCeroHastaN(int n){
    for (int i = 0; i <= n; i++){
        cout << i << endl;
    } 
}

int mult(int n, int m){

    if(n == 0 || m == 0){
        return 0;
    }
    else{
        if (n > 0 || m > 0){  
            int result = m;
            for (int i = n; i > 1; i--){
                result = result + m;
            }
            return result;
        }
        else {
            int result = m;
            for (int i = n; i > 1; i--){
                result = result + m;
            }
            return (0 - result);
        }
    }
}


void primerosN(int n, string s){
    for(int i = 0; i < n; i++){
        cout << s[i] << endl;
    }
}


int pertenece(char c, string s){
    int result = false;
    for(int i = s.length(); i >= 0; i--){
        if(s[i] == c){
            result = true;
        }
    }
    return result;
}


struct Fraccion {
    int numerador;
    int denominador;
};

Fraccion consFraccion(float n, float d){
    Fraccion f;
    f.numerador = n;
    f.denominador = d;

    return f;

}


int numerador(Fraccion f){
    return f.numerador;
}
int denominador(Fraccion f){
    return f.denominador;
}
int division(Fraccion f){
    return (f.numerador) / (f.denominador);
}
Fraccion multF(Fraccion f1, Fraccion f2){
    return (consFraccion((f1.numerador * f2.numerador), (f1.denominador * f2.denominador)));
}

int mcd(int num1, int num2){
    int res;
    int a = max(num1, num2);
    int b = min (num1, num2);

    do{
        res = b;
        b = a % b;
        a = res;
    } while (b != 0);


    return res;
}

int mcm(int num1, int num2){
    int res;
    res = (num1 * num2) / mcd(num1, num2);
    return res;
}

Fraccion sumF(Fraccion p, Fraccion e){
 Fraccion result;
 int maximoComun = mcm(p.denominador, e.denominador);
 result.denominador = maximoComun;
 result.numerador = ((p.denominador / maximoComun) * p.numerador) + ((e.denominador / maximoComun) * e.numerador);
 return result;
}

Fraccion simplificada(Fraccion p){
  Fraccion result = p;
  int n = result.numerador;
  int d = result.denominador;
  result.numerador = result.numerador / (mcd(n, d));
  result.denominador = result.denominador / (mcd(n, d));

  return result;
}



Fraccion sumF(Fraccion f1, Fraccion f2);


void mostrarFraccion(Fraccion f){
    cout << f.numerador << endl;
    cout << "___" << endl;
    cout << f.denominador << endl;
}

int main(){
    Fraccion f1 = consFraccion(2, 4);
    Fraccion f2 = consFraccion(8, 16); 
    mostrarFraccion(sumF(f1, f2));

    return 0;
};