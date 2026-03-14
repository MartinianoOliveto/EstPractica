import Map 

prueba = assocM 1 "A" $ assocM 2 "B" $ assocM 3 "C" $ emptyM

valuesM :: Eq k => Map k v -> [Maybe v]
--necesito la keys, para despues buscar su value en el map, con una subtarea 
valuesM m = valuesFromKeys (keys m) m 

valuesFromKeys :: Eq k => [k] -> Map k v -> [Maybe v]
--caso base 
valuesFromKeys [] _ = []
--caso recursivo 
valuesFromKeys (k:ks) m = (lookupM k m) : valuesFromKeys ks m 

todasAsociadas :: Eq k => [k] -> Map k v -> Bool  
--si uso keys, se que esa lista estan todas asociadas, solo comparo la primera lista con keys
--caso base 
todasAsociadas [] _ = True  
--caso recursivo
todasAsociadas (k:ks) m = if pertenece k (keys m)
                            then todasAsociadas ks m
                                else False 



--Biblioteca 
pertenece :: Eq a => a -> [a] -> Bool 
--caso base 
pertenece e [] = False 
--caso recursivo 
pertenece e (a:as) = if e == a 
                        then True 
                            else pertenece e as 