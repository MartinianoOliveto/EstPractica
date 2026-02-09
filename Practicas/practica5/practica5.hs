--el import siempre debe ser la primera linea de codigo 
import Set 
import Queue 
--los archivos que son modulos siempre empiezan con mayuscula 
--usuario del tipo abstracto
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--caso base 
losQuePertenecen [] s = []
--caso recursivo
losQuePertenecen (a:as) s = if belongs a s 
                                then a : losQuePertenecen as s 
                                    else losQuePertenecen as s 

unirTodos :: Eq a => Tree (Set a) -> Set a 
--caso base 
unirTodos EmptyT = emptyS
--caso recursivo 
unirTodos (NodeT x t1 t2) = unionS x (unionS (unirTodos t1) (unirTodos t2))


--Sets de prueba 
data Tree a = EmptyT | NodeT a (Tree a ) (Tree a)
    deriving (Eq, Show)

set1 :: Set Int 
set1 = addS 1 (addS 2 emptyS)

arbolSets :: Tree (Set Int)
arbolSets = NodeT (addS 1 (addS 2 emptyS)) -- raiz: 1,2 
                (NodeT (addS 3 emptyS) EmptyT EmptyT) -- hijo izquierdo: 3
                (NodeT (addS 4 (addS 5 emptyS))EmptyT EmptyT) --hijo derecho: 4,5




lengthQ :: Queue a -> Int 
lengthQ q = if not (isEmptyQ q)
                then 1 + lengthQ (dequeue q) 
                    else 0  

queueToList :: Queue a -> [a]
queueToList q = if not (isEmptyQ q)
                    then firstQ q : queueToList (dequeue q) 
                        else []

unionQ :: Queue a -> Queue a -> Queue a 
--caso base 
--caso recursivo 
unionQ q1 q2 = 

--Queue de prueba 
queue1 :: Queue Int 
queue1 = enqueue 1 (enqueue 2(enqueue 3 emptyQ))