--CALCULO DE COSTOS 
head' :: [a] -> a 
head' (x:xs) = x 
{-Costo: O(1) (constante)
Observacion: no depende de los elementos de la estructura siempre toma el primero y descarta el resto
-}
sumar :: Int -> Int 
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
{- Costo : O(1) (constante)
Observacion: solo suma 1 a x, no depende de los elementos de la estructura 
-}
factorial :: Int -> Int 
factorial 0 = 1 
factorial n = n * factorial (n-1)
{-Costo: O(n) (lineal)
Observacion: por cada elemento de la estructura, hace una operacion de costo constante (la multiplicacion)
-}
longitud :: [a] -> Int 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs 
{-Costo: 0(n) (lineal)
Observacion: por cada elemento de la estructura, hace una operacion de costo constante (suma 1 por cada elemento)
-}
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs 
-- esto lo que hace dada [Int] devuelve [Int] que contiene los factoriales de todos los elementos
{-Costo: O(n^2) 
Observacion: por cada elemento de la lista, aplico factorial, que es de costo O(n)
-}
pertenece :: Eq a => a -> [a] -> Bool
pertence n [] = False 
pertenece n (x:xs) = n == x || pertenece n xs 
{-Costo: O(n) (lineal)
por cada elemento de la lista, aplica una operacion de nivel O(1) (el ==)
-}
sinRepetidos :: Eq a => [a] -> [a] 
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertence x xs 
                        then sinRepetidos xs 
                            else x : sinRepetidos xs 
{-Costo: lineal en el peor de los casos??, constante en el mejor??
Observacion: si el primer elemento de la lista es el que busco, seria O(1)
            Ahora; si no es el primer elemento el que busco, ahi aplico una operacion constante en los elementos
            que hagan falta, por lo tanto seria O(n)
-}
append :: [a] -> [a] -> [a]
append [] ys = ys 
append (x:xs) ys = x : append xs ys 
{-Costo: constante en el mejor de los casos, lineal en el peor?
Observaciones: si la primer lista es vacia, devuelve ys, o sea es una operacion de costo constante O(1)
                Ahora; si xs tiene al menos 1 elemento, aplico una operacion de costo constante O(n)
-}
concatenar :: [String] -> String 
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs 
{-Costo: constante en el mejor de los casos, lineal en el peor?
Observaciones: Si la lista es vacia, solo la devuelvo O(1)
                Pero, si la lista tiene al menos 1 elemento, tengo que concatenar c/u, seria O(n)
-}
takeN :: Int -> [a] ->[a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs 