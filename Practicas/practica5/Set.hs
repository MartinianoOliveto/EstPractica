--la primera linea de codigo siempre debe ser el module 
--hay que agregar el tipo entre parentesis, asi otros archivos pueden verlo
module Set (Set,emptyS, belongs, addS, sizeS, removeS, unionS, setToList) where 

--Set 
data Set a = S [a]
    deriving (Eq, Show)

emptyS :: Set a 
emptyS = S []

belongs :: Eq a => a -> Set a -> Bool
--caso base  
belongs a (S []) = False 
--caso recursivo 
belongs a (S(x:xs)) = a == x || belongs a (S xs) 

addS :: Eq a => a -> Set a -> Set a 
addS a (S xs) = if belongs a (S xs)
            then (S xs)
                else S (a : xs)

sizeS :: Eq a => Set a -> Int 
--caso base 
sizeS (S []) = 0 
--caso recursivo 
sizeS (S (x:xs)) = 1 + sizeS (S xs)

removeS :: Eq a => a -> Set a -> Set a 
--caso base 
removeS a (S[]) = error "El elemento no esta en el conjunto"
--caso recursivo 
removeS a (S (x:xs)) = if a == x 
                        then (S xs)
                            else removeS a (S xs)

unionS :: Eq a => Set a -> Set a -> Set a 
--caso base 
unionS (S []) (S bs) = (S bs) 
unionS (S as) (S []) = (S as) 
--caso recursivo 
unionS (S (a:as)) (S bs) = addS a (unionS (S as) (S bs))

setToList :: Eq a => Set a -> [a] 
{-Describe una lista con todos los elementos distintos del conjunto, pero en el Set no puede haber repetidos 
Por lo tanto, debido a la implementacion que hice, solo debo pasar de Set a una lista, ya que a la hora de agregar
cumplo que no haya repetidos 
-}
--caso base 
setToList (S []) = []
--caso recursivo 
setToList (S (x:xs)) = x : setToList (S(xs))