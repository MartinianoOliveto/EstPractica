module PriorityQueue(PriorityQueue, emptyPQ, isEmptyPQ, insertPQ,findMinPQ,deleteMinPQ)
    where 

--NOTA: el minimo es el que tiene mayor prioridad 
-- en esta implementacion, es mas costoso insertar, pero mas barato borrar 

data PriorityQueue a = PQ [a]
    deriving(Eq, Ord, Show)

emptyPQ :: PriorityQueue 
--costo O(1)
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue -> Bool 
--costo O(1)
isEmptyPQ (PQ []) = True 
isEmptyPQ (PQ _) = False 

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a 
--mismo costo que insertarEnPQ 
--caso base 
insertPQ a (PQ []) = (PQ [a])
--caso recursivo 
insertPQ a (PQ bs) = (PQ insertarEnPQ a bs)

insertarEnPQ :: Ord a => a -> [a] -> [a]
--dado un elemento a y [a], lo inserta siguiendo la logica de una PQ 
--costo: O(n) siendo n los elementos de la lista 
--caso base 
insertarEnPQ a [] = [a]
--caso recursivo
insertarEnPQ a (b:bs) = if a <= b 
                            then (a:b:bs)
                                else b : insertarEnPQ a bs 

findMinPQ :: Ord a => PriorityQueue a -> a
--costo: O(1)
findMinPQ (PQ []) = error "No leiste la precondicion"
findMinPQ (PQ as) = (PQ head as) 

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a 
--costo O(1)
deleteMinPQ (PQ []) = error "No leiste la precondicion"
deleteMinPQ (PQ as) = (PQ tail as) 
