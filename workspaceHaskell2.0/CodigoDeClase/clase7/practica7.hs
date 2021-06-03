

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

tree1= NodeT 50 (NodeT 25 (NodeT 10 (EmptyT) (EmptyT)) (NodeT 49 (EmptyT) (EmptyT))) (NodeT 60 (EmptyT) (EmptyT))
tree2= NodeT 50 (NodeT 25 (NodeT 10 (EmptyT) (EmptyT)) (NodeT 49 (NodeT 23 (EmptyT) (NodeT 28 (NodeT 22 (NodeT 21 (EmptyT) (EmptyT)) (EmptyT)) (EmptyT))) (EmptyT))) (NodeT 60 (EmptyT) (EmptyT))

balanceado :: Tree a -> Bool
balanceado (NodeT x ti td) = (abs (profundidad ti - profundidad td)) <= 1


profundidad :: Tree a -> Int
profundidad EmptyT = 0
profundidad (NodeT x ti td) = 1 + max (profundidad ti) (profundidad td)
-- Indica si el árbol está balanceado. Un árbol está balanceado cuando para cada nodo la
-- diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.




insertBST :: Ord a => a -> Tree a -> Tree a
insertBST a EmptyT          = (NodeT a EmptyT EmptyT) 
insertBST a (NodeT x ti td) = if ( a == x) then (NodeT x ti td) else if (a > x) then (NodeT x ti (insertBST a td)) else (NodeT x (insertBST a ti) td)

-- Dado un BST inserta un elemento en el árbol.
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST a (NodeT x EmptyT EmptyT) = x
deleteBST a (NodeT x ti td) = if a == x then juntar ti td else if a < x then (NodeT x (deleteBST a ti) td) else (NodeT x ti (deleteBST a td))   
-- Dado un BST borra un elemento en el árbol.
perteneceBST :: Ord a => a -> Tree a -> Bool
perteneceBST a (NodeT x EmptyT EmptyT) = a == x
perteneceBST a (NodeT x ti td)         = a == x || if a > x then perteneceBST a td else perteneceBST a ti
-- Dado un BST dice si el elemento pertenece o no al árbol.
splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST bst = (elementoMin bst, deleteBST (elementoMin bst))


elementoMin :: Tree a -> a
elementoMin (NodeT x EmptyT EmptyT) = x
elementoMin (NodeT x ti td)         = elementoMin ti
-- Dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST = undefined
-- Dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA = undefined
-- Dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado.
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA = undefined
-- Dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado.