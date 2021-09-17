#include <iostream>
#include "arbolesBinarios.h"

using namespace std;

struct NodeT{
    int elem;
    NodeT* left;
    NodeT* right;
};

Tree emptyT(){
    return NULL;
}

Tree nodeT(int elem, Tree left, Tree right){
    NodeT* node = new NodeT;
    node->elem = elem;
    node->left = left;
    node->right = right;

    return node;
}

bool isEmptyT(Tree t){
    return t == NULL;
}

int rootT(Tree t){
    return t->elem;
}

Tree left(Tree t){
    return t->left;
}

Tree right(Tree t){
    return t->right;
}