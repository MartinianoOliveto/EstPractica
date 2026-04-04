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

--3 nave espacial 
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible  
    deriving Show 
data Sector = S SectorId [Componente] [Tripulante]
    deriving Show 
type SectorId = String 
type Tripulante = String 

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show 
data Nave = N (Tree Sector)
    deriving Show

--1
sectores :: Nave -> [SectorId]
sectores (N ts) = sectoresT ts 

sectoresT :: Tree Sector -> [SectorId]
--caso base 
sectoresT EmptyT = []
--caso recursivo
sectoresT (NodeT s t1 t2) = idSector s : sectoresT t1 ++ sectoresT t2 

idSector :: Sector -> SectorId 
idSector (S id _ _) = id 
------------------------------------------------------------------
poderDePropulsion :: Nave -> Int 
poderDePropulsion (N ts) = poderDePropulsionT ts 

poderDePropulsionT :: Tree Sector -> Int
--caso base 
poderDePropulsionT EmptyT = 0 
--caso recursivo  
poderDePropulsionT (NodeT s t1 t2) = poderDePropulsionS s + poderDePropulsionT t1 
                                    + poderDePropulsionT t2 

poderDePropulsionS :: Sector -> Int 
poderDePropulsionS (S _ cs _) = poderDePropulsionC cs 

poderDePropulsionC :: [Componente] -> Int
--caso base 
poderDePropulsionC [] = 0 
--caso recursivo  
poderDePropulsionC (c:cs) = if esMotor c 
                                then poder c + poderDePropulsionC cs 
                                else poderDePropulsionC cs 

esMotor :: Componente -> Bool 
esMotor (Motor _) = True 
esMotor _ = False 

poder :: Componente -> Int 
poder (Motor p) = p 
----------------------------------------------------------------
barriles :: Nave -> [Barril]
barriles (N ts) = barrilesT ts 

barrilesT :: Tree Sector -> [Barril]
barrilesT EmptyT = []
barrilesT (NodeT s t1 t2) = barrilesS s ++ barrilesT t1 ++ barrilesT t2  

barrilesS :: Sector -> [Barril]
barrilesS (S _ cs _) = barrilesC cs 

barrilesC :: [Componente] -> [Barril]
barrilesC [] = []
barrilesC (c:cs) = if esAlmacen c 
                    then listaBarriles c ++ barrilesC cs 
                    else barrilesC cs 

esAlmacen :: Componente -> Bool 
esAlmacen (Almacen bs) = True 
esAlmacen _ = False 

listaBarriles :: Componente -> [Barril]
listaBarriles (Almacen bs) = bs 
---------------------------------------------------------------------------------------------
agregarASector :: [Componente] -> SectorId -> Nave -> Nave 
agregarASector cs sid (N ts) = (N (agregarASectorT cs sid ts))

agregarASectorT :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
--caso base 
agregarASectorT cs sid EmptyT = EmptyT
--caso recursivo
agregarASectorT cs sid (NodeT s t1 t2) = if mismoSector sid (idSector s)
                                            then (NodeT (agregarASectorS cs s)
                                                    t1 t2)
                                            else (NodeT s (agregarASectorT cs sid t1)
                                                    (agregarASectorT cs sid t2))

agregarASectorS :: [Componente] -> Sector -> Sector 
agregarASectorS cs (S sid cs' a) = (S sid (cs'++cs) a)

mismoSector :: SectorId -> SectorId -> Bool 
mismoSector id1 id2 = id1 == id2 
------------------------------------------------------------------------
nave1 :: Nave
nave1 =N(NodeT(S "Puente"
                [Motor 10, LanzaTorpedos]
                ["Kirk","Spock"])
            (NodeT(S "Ingenieria"
                    [Motor 50, Almacen [Combustible, Oxigeno]]
                    ["Scotty"])
                EmptyT
                EmptyT)
            (NodeT(S "Deposito"
                    [Almacen [Comida, Torpedo, Torpedo]]
                    ["Chekov"])
                EmptyT
                EmptyT)
            )

esIgual ::Eq a => a -> [a] -> Bool 
esIgual a [] = False 
esIgual a (b:bs) = a == b || esIgual a bs 

--lobos 
type Presa = String --nombre de presa 
type Territorio = String --nombre de territorio 
type Nombre = String --nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo |
            Explorador Nombre [Territorio] Lobo Lobo |
            Cria Nombre 
                deriving Show 
data Manada = M Lobo 
    deriving Show 

--2 
buenaCaza :: Manada -> Bool 
buenaCaza m =  cantPresas m > cantCrias m  

cantCrias :: Manada -> Int 
cantCrias (M l) = cantCriasL l 

cantCriasL :: Lobo -> Int 
--caso base 
cantCriasL (Cria _) = 1 
--caso recursivo 
cantCriasL (Cazador _ _ l1 l2 l3) = cantCriasL l1 + cantCriasL l2 + cantCriasL l3  
cantCriasL (Explorador _ _ l1 l2) = cantCriasL l1 + cantCriasL l2 

cantPresas :: Manada -> Int 
cantPresas (M l) = cantPresasL l 

cantPresasL :: Lobo -> Int
--caso base 
cantPresasL (Cria _) = 0
--caso recursivo 
cantPresasL (Cazador _ ls l1 l2 l3) = length ls + cantPresasL l1 + cantPresasL l2 
                                        + cantPresasL l3
cantPresasL (Explorador _ _ l1 l2) = cantPresasL l1 + cantPresasL l2 
--------------------------------------------------------------------------------------
{-elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaL l 

elAlfaL :: Lobo -> (Nombre, Int)
--caso base 
elAlfaL (Cria n) = (n,0)
--caso recursivo 
--el explorador tiene 2 hijos, busco al alfa ahi 
elAlfaL (Explorador _ _ l1 l2) = elQueCazoMasEntre (elAlfaL l1) (elAlfaL l2)
--debo comprarar si cazo mas el lobo actual, o sus hijos
elAlfaL (Cazador n ls l1 l2 l3) = (n,length ls) > cantDePresas -}

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elQueMasCazo (tuplasDeLobos l)

tuplasDeLobos :: Lobo -> [(Nombre, Int)]
--caso base 
tuplasDeLobos (Cria n) = [(n,0)]
--caso recursivo 
tuplasDeLobos (Cazador n ls l1 l2 l3) = (n, length ls) : tuplasDeLobos l1 ++ 
                                        tuplasDeLobos l2 ++ tuplasDeLobos l3 
tuplasDeLobos (Explorador n _ l1 l2) = (n,0) : tuplasDeLobos l1 ++ tuplasDeLobos l2 

elQueMasCazo :: [(Nombre, Int)] -> (Nombre, Int)
elQueMasCazo (lp:lps) = elQueMasCazoEntre lp lps

elQueMasCazoEntre :: (Nombre, Int) -> [(Nombre, Int)] -> (Nombre, Int)
elQueMasCazoEntre x [] = x
elQueMasCazoEntre (a,b) ((l,p):lps) = if b > p
                                        then elQueMasCazoEntre (a,b) lps 
                                        else elQueMasCazoEntre (l,p) lps 



manada1 :: Manada
--manada un solo cazador, mala caza 
manada1 =M(Cazador "Akela" ["ciervo","conejo"]
            (Explorador "Raksha" ["Norte"]
              (Cria "Luna")
              (Cria "Rayo")
            )
            (Cazador "Kiba" ["ciervo","jabalí","conejo"]
              (Cria "Sombra")
              (Cria "Nube")
              (Cria "Colmillo")
            )
            (Cria "Chispa")
        )
--manada con varios cazadores, buena caza 
manada2 :: Manada
manada2 =M(Cazador "Thor" ["ciervo","conejo"]
            (Cazador "Loki" ["jabalí"]
              (Cria "Luna")
              (Cria "Rayo")
              (Cria "Sombra")
            )
            (Explorador "Freya" ["Norte"]
              (Cazador "Fenrir" ["ciervo","jabalí","conejo","oveja"]
                  (Cria "Nube")
                  (Cria "Colmillo")
                  (Cria "Humo")
              )
              (Cria "Bruma")
            )
            (Cazador "Odin" ["conejo","conejo","ciervo","ciervo"]
              (Cria "Chispa")
              (Cria "Fuego")
              (Cria "Eco")
            )
        )
manada3 :: Manada
--manada con cazadores en 0 presas  
manada3 =M(Explorador "Atlas" ["Sur"]
            (Cria "Luna")
            (Cazador "Rex" []
              (Cria "Nube")
              (Cria "Sombra")
              (Cria "Rayo")
            )
        )