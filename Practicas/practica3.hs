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
