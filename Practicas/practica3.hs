--1 tipos recursivos simples 
--1.1 celdas con bolitas 

data Color = Azul | Rojo
    deriving (Show, Eq)
data Celda = Bolita Color Celda | CeldaVacia 
    deriving Show

--celdas de prueba 
dosAzules = Bolita Azul(Bolita Azul CeldaVacia)



nroBolitas :: Color -> Celda -> Int 
nroBolitas x CeldaVacia = 0 
nroBolitas x (Bolita co ce) = if x == co
                                then 1 + nroBolitas x ce
                                    else nroBolitas x ce 

poner :: Color -> Celda -> Celda 
poner x CeldaVacia = (Bolita x CeldaVacia)
poner x (Bolita co ce) = (Bolita x(Bolita co ce))

sacar :: Color -> Celda -> Celda 
--en el caso base, si devuelvo celdaVacia, saco todos los colores
--esto quiere decir que no encontre el color y saque todos 
sacar x CeldaVacia = CeldaVacia 
sacar x (Bolita co ce) = if x == co
                            then ce 
                                else sacar x ce 
-- lo que hace esta funcion es sacar el color si lo encuentra, si no saca todos 
--aunque la funcion en este caso es total, deberia devolver la celda original, no vaciarla 
--esto podria solucionarlo con una subtarea??

--falta sacar y ponerN 

--1.2 camino hacia el tesoro
data Objeto = Cacharro | Tesoro 
    deriving Show 
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
    deriving Show 

--ejemplos para usar 
caminoDeCacharros = Cofre [Cacharro] (Nada (Cofre [Cacharro, Cacharro] Fin))
caminoVacio = Nada (Nada (Nada Fin))
aventura = Cofre [Cacharro] (Nada (Cofre [Tesoro, Tesoro] (Nada Fin)))
caminoSimple = Cofre [Tesoro] Fin

hayTesoro :: Camino -> Bool 
hayTesoro Fin = False 
hayTesoro (Cofre obs c) = if algunTesoro obs 
                            then True 
                                else hayTesoro c 
hayTesoro (Nada c) = hayTesoro c 

pasosHastaElTesoro :: Camino -> Int 
pasosHastaElTesoro Fin = error"No leiste la precondicion"
pasosHastaElTesoro (Cofre obs c) = if algunTesoro obs 
                                    then 0 
                                        else 1 + pasosHastaElTesoro c 
pasosHastaElTesoro (Nada c) = 1 + pasosHastaElTesoro c 

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn x c = pasosHastaElTesoro c == x 

alMenosNTesoros :: Int -> Camino -> Bool 
alMenosNTesoros x c = cantDeTesorosEn c >= x 

--subtareas 
algunTesoro :: [Objeto] -> Bool 
--dada una lista de objetos indica si hay al menos un tesoro 
algunTesoro [] = False 
algunTesoro (ob:obs) = if esTesoro ob 
                            then True 
                                else algunTesoro obs 

esTesoro :: Objeto -> Bool 
--dado un objeto indica si es tesoro 
esTesoro Tesoro = True
esTesoro Cacharro = False 

cantDeTesorosEn :: Camino -> Int 
--dado un camino devuelve la cantidad de tesoros que hay 
cantDeTesorosEn Fin = 0
cantDeTesorosEn (Cofre obs c) = tesorosEn obs + cantDeTesorosEn c 
cantDeTesorosEn (Nada c) = cantDeTesorosEn c

tesorosEn :: [Objeto] -> Int 
--dada una lista de objts devuelve la cantidad de tesoros 
tesorosEn [] = 0 
tesorosEn (ob:obs) = if esTesoro ob 
                        then 1 + tesorosEn obs 
                            else tesorosEn obs 

--2 tipos arboreos 
--2.1 arboles binarios 
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show 
--arboles de prueba 
arbolSimple :: Tree Int --con esto se fuerza el tipo a Int, Haskell lo tomaba como Integer     
arbolSimple = NodeT 10 
                (NodeT 5 EmptyT EmptyT) 
                    (NodeT 15 EmptyT EmptyT)
arbolComplejo :: Tree Int
arbolComplejo = NodeT 50
                    (NodeT 25
                        (NodeT 12 EmptyT EmptyT)
                        (NodeT 35 EmptyT EmptyT)
                    )
                    (NodeT 75
                        (NodeT 60 EmptyT EmptyT)
                        (NodeT 90 EmptyT EmptyT)
                    )
arbolRepetidos :: Tree Int 
arbolRepetidos = NodeT 0 
                    (NodeT 0
                        (NodeT 0 EmptyT EmptyT)
                        (NodeT 0 EmptyT EmptyT)
                    )
                    (NodeT 0
                        (NodeT 0 EmptyT EmptyT)
                        (NodeT 0 EmptyT EmptyT)
                    )
arbolDispar :: Tree Int 
arbolDispar = NodeT 100
                (NodeT 50
                    (NodeT 25
                        (NodeT 12 EmptyT EmptyT) -- height de 4 
                    EmptyT)
                EmptyT)
                (NodeT 150 EmptyT EmptyT) -- height de 2 

--1                      
sumarT :: Tree Int -> Int 
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2 
--2
sizeT :: Tree a -> Int 
sizeT EmptyT = 0
sizeT (NodeT _ t1 t2) = 1 + sizeT t1 + sizeT t2
--3 
mapDobleT :: Tree Int -> Tree Int 
mapDobleT EmptyT = EmptyT 
mapDobleT (NodeT n t1 t2) = (NodeT (n*2) (mapDobleT t1) (mapDobleT t2))
--4 
perteneceT :: Eq a => a -> Tree a -> Bool 
perteneceT a EmptyT = False 
perteneceT a (NodeT b t1 t2) = if a == b 
                                then True 
                                    else (perteneceT a t1) || (perteneceT a t2) 
--5 
aparicionesT :: Eq a => a -> Tree a -> Int 
aparicionesT a EmptyT = 0
aparicionesT a (NodeT b t1 t2) = if a == b 
                                    then 1 + (aparicionesT a t1) + (aparicionesT a t2)
                                        else (aparicionesT a t1) + (aparicionesT a t2)
--6
leaves :: Tree a -> [a]
leaves EmptyT = []
--si es un nodo con 2 hijos vacios es una hoja
leaves (NodeT a EmptyT EmptyT) = [a]
leaves (NodeT a t1 t2) = leaves t1 ++ leaves t2 
--7 
heightT :: Tree a -> Int 
heightT EmptyT = 0 
heightT (NodeT _ t1 t2) = 1 + max (heightT t1) (heightT t2)
--8 
mirrorT :: Tree a -> Tree a 
mirrorT EmptyT = EmptyT 
mirrorT (NodeT a t1 t2) = (NodeT a (mirrorT t2) (mirrorT t1))
--9 
toList :: Tree a -> [a]
--primero se recorre la rama izq, raiz y derecha 
toList EmptyT = []
toList (NodeT n t1 t2) = toList t1 ++ n : toList t2 -- esto funciona, pero como sabe haskell como tiene que ordenarlo?
--toList (NodeT n t1 t2) = toList t1 ... n ... toList t2 
------------------------------------ ++ --- :: ----------- aca pense, si agrego n a t2 solo me queda hacer append con t1 
--10
levelN :: Int -> Tree a -> [a]
--contemplo si el arbol es vacio no hay nada 
levelN _ EmptyT = []
--si el nivel es 0 devuelvo lo que haya 
levelN 0 (NodeT a _ _) = a : []
--caso recursivo 
levelN x (NodeT a t1 t2) = if x == 0
                            then a : []
                                else levelN (x-1) t1 ++ levelN (x-1) t2
{-tratando de reescribir el caso recursivo con PM 
levelN x (NodeT a t1 t2) = case a of 
                                a > 0 -> levelN (x-1) t1 ++ levelN (x-1) t2 
                                a == 0 -> a : []
en a > 0 da error de parseo, tal vez se puede resolver con guardas, pero no hay ejemplos en el pdf de PM con guardas-}
-- no se pudio jejejje 
--11
listPerLevel :: Tree a -> [[a]] 
--caso base 
listPerLevel EmptyT = []
--caso recursivo 
listPerLevel (NodeT a t1 t2) = [[a]] ++ listPerLevel t1 ++ listPerLevel t2 
--no devuelve el resultado esperado, solo una lista de listas pero no ordenada por niveles 
--12
ramaMasLarga :: Tree a -> [a]
--caso base 
ramaMasLarga EmptyT = []
--caso recursivo
{--si tomo la rama mas larga una vez, voy a devolver todos los elementos de todas las ramas restantes
ahi entra la recursion, debo elegir en todos los niveles la rama mas larga, asi estaria eligiendo el 
"camino" mas largo--}
ramaMasLarga (NodeT a t1 t2) = a : ramaMasLarga (ramaMasLargaEntre t1 t2)
ramaMasLargaEntre :: Tree a -> Tree a -> Tree a 
--dados dos arboles, devuelve el mas largo
ramaMasLargaEntre a b = if heightT a > heightT b 
                            then a 
                                else b 
--13 hacerlo despues, tiene pinta de que es complicadito 
--2.2 Expresiones aritmeticas
--lo que hace esto es plantear operaciones aritemticas como un arbol?
data ExpA = Valor Int 
            | Sum ExpA ExpA
            | Prod ExpA ExpA
            | Neg ExpA
--1
eval :: ExpA -> Int 
--esto seria el caso base, cuando llego al valor
eval (Valor x) = x 
--caso recursivo
eval (Sum x y) = eval x + eval y 
eval (Prod x y) = eval x * eval y 
eval (Neg x) = (-1) * eval x 
--2
--no lo entendi xd 