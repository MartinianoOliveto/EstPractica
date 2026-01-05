--1 recursion sobre listas 
--1
sumatoria :: [Int] -> Int
--caso base 
sumatoria [] = 0 
--caso recursivo
sumatoria (n:ns) = n + sumatoria ns 
--2 
longitud :: [a] -> Int 
--caso base 
longitud [] = 0 
--caso recursivo 
longitud (a:as) = 1 + longitud as 
--3 
sucesores :: [Int] -> [Int]
--caso base 
sucesores [] = []
--caso recursivo 
sucesores (n:ns) = (n+1) : sucesores ns 
--4
conjuncion :: [Bool] -> Bool 
--caso base 
conjuncion [] = True -- si llegaste hasta aca, quiere decir que todos son True, por lo tanto
--caso recursivo
conjuncion (b:bs) = if b == True
                        then conjuncion bs  
                            else False 
--5 
disyuncion :: [Bool] -> Bool 
--caso base 
disyuncion [] = False -- si llegaste hasta aca, quiere decir que todos son False, por lo tanto 
--caso recursivo 
disyuncion (b:bs) = if b == False 
                        then disyuncion bs 
                            else True 
--6 
{-aplanar :: [[a]] -> [a]
--caso base 
aplanar [[]] = []
--caso recursivo
aplanar (a:as) = a : aplanar as --a es una lista, por lo tanto que tengo que hacer aca??-}
--7 
pertenece :: Eq a => a -> [a] -> Bool 
--caso base 
pertenece e [] = False 
--caso recursivo 
pertenece e (a:as) = if e == a 
                        then True 
                            else False 
--8 
apariciones :: Eq a => a -> [a] -> Int 
--caso base 
apariciones e [] = 0
--caso recursivo
apariciones e (a:as) = if e == a 
                        then 1 + apariciones e as 
                            else apariciones e as  
--9 
losMenoresA :: Int -> [Int] -> [Int]
--caso base 
losMenoresA x [] = []
--caso recursivo
losMenoresA x (n:ns) = if n < x 
                        then n : losMenoresA x ns 
                            else losMenoresA x ns 
--10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
--caso base 
lasDeLongitudMayorA x [] = []
--caso recursivo
lasDeLongitudMayorA x (a:as) = if longitud a > x
                                    then a : lasDeLongitudMayorA x as 
                                        else lasDeLongitudMayorA x as  
--11 
agregarAlFinal :: [a] -> a -> [a]
--caso base
agregarAlFinal [] a = [a] -- cuando la lista sea vacia, agrego el elemento
--caso recursivo
agregarAlFinal (x:xs) a =  x : agregarAlFinal xs a --tengo que ir agregando la cabeza de lista
--12 
agregar :: [a] -> [a] -> [a]
--caso base 
agregar [] [] = []
agregar x [] = x 
agregar [] y = y
--caso recursivo
agregar (x:xs) ys = x : agregar xs ys 