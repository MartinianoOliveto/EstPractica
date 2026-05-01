data Sanguche = Pan Relleno 
    deriving Show 
data Relleno = Feta TipoDeFeta Relleno 
                | Aire 
    deriving Show 
data TipoDeFeta = Queso | Jamon | Mortadela | Salame 
    deriving Show 

--a 
rellenoDeAire :: Sanguche -> Bool 
rellenoDeAire (Pan r) = esAire r 

esAire :: Relleno -> Bool 
esAire Aire = True 
esAire _ = False 
--b
esTortitaDeJamon :: Sanguche -> Bool 
esTortitaDeJamon (Pan r) = todoJamon r 

todoJamon :: Relleno -> Bool 
--caso base 
todoJamon Aire = True 
--caso recursivo 
todoJamon (Feta f r) = esJamon f && todoJamon r 

esJamon :: TipoDeFeta -> Bool 
esJamon Jamon = True 
esJamon _ = False 
--c 
mandaleNDe :: Int -> TipoDeFeta -> Sanguche -> Sanguche
mandaleNDe n f (Pan r) = (Pan (mandaleNDeR n f r))

mandaleNDeR :: Int -> TipoDeFeta -> Relleno -> Relleno
mandaleNDeR 0 _ r = r 
mandaleNDeR n f r = Feta f (mandaleNDeR (n-1) f r)
--d
peroSinQueso :: Sanguche -> Sanguche 
peroSinQueso (Pan r) = (Pan (sinQuesoR r)) 

sinQuesoR :: Relleno -> Relleno
sinQuesoR Aire = Aire 
sinQuesoR (Feta f r) = if esQueso f 
                        then sinQuesoR r 
                        else (Feta f (sinQuesoR r))

esQueso :: TipoDeFeta -> Bool 
esQueso Queso = True 
esQueso _ = False 
--e 
ordenadosPorCantidad :: Sanguche -> [(TipoDeFeta, Int)]
ordenadosPorCantidad (Pan r) = ordenarT (ordenadosPorCantidadR r)

ordenadosPorCantidadR :: Relleno -> [(TipoDeFeta, Int)]
--caso base 
ordenadosPorCantidadR Aire = []
--caso recursivo
ordenadosPorCantidadR (Feta f r) = armarTuplasF (f,1) (ordenadosPorCantidadR r)

armarTuplasF :: (TipoDeFeta, Int) -> [(TipoDeFeta, Int)] -> [(TipoDeFeta, Int)] 
--caso base 
armarTuplasF (f,n) [] = (f,n) : []
--caso recursivo
armarTuplasF (f,n) ((f',n'):fns) = if esMismaFeta f f'
                                        then ((f,(n'+n)):fns)
                                        else (f',n') : armarTuplasF (f,n) fns 

esMismaFeta :: TipoDeFeta -> TipoDeFeta -> Bool 
esMismaFeta Jamon Jamon = True 
esMismaFeta Queso Queso = True 
esMismaFeta Mortadela Mortadela = True 
esMismaFeta Salame Salame = True 
esMismaFeta _ _ = False 

ordenarT :: [(TipoDeFeta, Int)] -> [(TipoDeFeta, Int)] 
ordenarT [] = []
ordenarT ((f,n):fns) = ordenar (f,n) (ordenarT fns)

ordenar :: (TipoDeFeta, Int) -> [(TipoDeFeta, Int)] -> [(TipoDeFeta, Int)] 
--caso base 
ordenar (f,n) [] = (f,n) : []
--caso recursivo
ordenar (f,n) ((f',n'):fns) = if n >= n'
                                then (f,n) : (f',n') : fns 
                                else (f',n') : ordenar (f,n) fns 

