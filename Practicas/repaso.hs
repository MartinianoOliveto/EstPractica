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


