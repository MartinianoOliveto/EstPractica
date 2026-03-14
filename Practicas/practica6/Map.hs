module Map(Map, emptyM, assocM, lookupM, deleteM, keys)
    where 
--NOTA: un map es un diccionario, relaciona una key a uno o varios values 

data Map k v = M [(k,v)]
    deriving (Eq, Show)

emptyM :: Map k v 
emptyM = M []

assocM :: Eq k => k -> v -> Map k v -> Map k v 
assocM k v (M kvs) = M((k,v):kvs)

{-lookupM :: Eq k => k -> Map k v -> Maybe v
--caso base 
lookupM k' (M []) = Nothing 
--caso recursivo 
lookupM k' (M((k,v):kvs)) = if k' == k 
                            then Just v 
                                else lookupM k' (M kvs)-}
--si bien esto funciona, queda mejor hacer una subtarea que trabaje con la lista 

lookupM :: Eq k => k -> Map k v -> Maybe v 
lookupM k' (M kvs) = lookupMList k' kvs 

lookupMList :: Eq k => k -> [(k,v)] -> Maybe v 
--caso base 
lookupMList k' [] = Nothing 
--caso recursivo 
lookupMList k' ((k,v):kvs) = if k' == k 
                                then Just v 
                                    else lookupMList k' kvs 

deleteM :: Eq k => k -> Map k v -> Map k v 
deleteM k' (M kvs) = M (deleteMList k' kvs) 

deleteMList :: Eq k => k -> [(k,v)] -> [(k,v)]
--caso base 
deleteMList k' [] = error "La clave no esta asociada a ningun valor"
--caso recursivo 
deleteMList k' ((k,v):kvs) = if k' == k
                                then kvs 
                                    else (k,v) : deleteMList k' kvs

keys :: Map k v -> [k]
keys (M kvs) = keysList kvs

keysList :: [(k,v)] -> [k]
--caso base 
keysList [] = []
--caso recursivo 
keysList ((k,v):kvs) = k : keysList kvs 
prueba = assocM 1 "A" $ assocM 2 "B" $ assocM 3 "C" $ emptyM
--prueba :: Map Int 
prueba = assocM 1 3 $ assocM 2 4 $ assocM 6 8 $ emptyM 