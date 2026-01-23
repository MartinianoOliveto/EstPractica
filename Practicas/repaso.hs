tomarHasta :: Int -> [a] -> [a]
--caso base 
tomarHasta _ [] = []
tomarHasta 0 _ = [] -- no necesito preguntar con un if else cuando llegue a cero, lo resuelvo con PM
--caso recursivo
tomarHasta x (a:as) = a : tomarHasta (x-1) as  

tomarDesde :: Int -> [a] ->[a]
--caso base 
tomarDesde _ [] = []
tomarDesde 0 as = as
--caso recursivo 
tomarDesde x (a:as) = tomarDesde (x-1) as 

tomarEntre :: Int -> Int -> [a] -> [a]
tomarEntre i j xs = tomarHasta (j-i+1) (tomarDesde i xs)

apariciones :: Eq a => [a] -> [(a,Int)]
--dada una lista, devuelve una tupla con cada elemento y las veces que este aparece 
--caso base 
apariciones [] = []
--caso recursivo
apariciones (x:xs) = agregar x (apariciones xs) 

agregar :: Eq a => a -> [(a,Int)] -> [(a,Int)]
agregar x [] =(x,1) : []
agregar x ((y,n):yns) = if x == y 
                            then (y,n+1) : yns --en este caso ya esta en la lista
                                else (y,n) : agregar x yns --en este caso no esta en la lista 

indexar :: [a] -> [(Int,a)]
--caso base
indexar [] = []
--caso recursivo
indexar (x:xs) = (0,x) : aumentarIndice (indexar xs) 

aumentarIndice :: [(Int, a)] -> [(Int,a)]
aumentarIndice [] = []
aumentarIndice ((i,x):ixs) = (i+1, x) : aumentarIndice ixs 

--ejemplo con booleanos
hayMayorDeEdad :: [Persona] -> Bool 
hayMayorDeEdad [] = False
hayMayorDeEdad(p:ps) = esMayorDeEdad p || hayMayorDeEdad ps 
--no usa condicionales 