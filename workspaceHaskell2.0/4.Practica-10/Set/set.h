#include <iostream>
#include "linkedlist.h"

using namespace std;

struct NodoS;

struct SetSt;

typedef SetSt* Set;

Set emptyS();

bool isEmptyS(Set s);

bool belongsS(int x, Set s);

void addS(int x, Set s);

void removeS(int x, Set s);

int sizeS(Set s);

LinkedList setToList(Set s);

void destroyS(Set s);