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
--
--caso base 
--caso recursivo 
--
--7 
pertenece :: Eq a => a -> [a] -> Bool 
--caso base 
pertenece e [] = False 
--caso recursivo 
pertenece e (a:as) = if e == a 
                        then True 
                            else pertenece e as 
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
--13 
reversa :: [a] ->[a]
--caso base 
reversa [] = []
--caso recursivo
reversa (x:xs) = agregarAlFinal (reversa xs) x 
--14
zipMaximos :: [Int] -> [Int] -> [Int]
--caso base 
zipMaximos [] [] = error "Ambas listas no pueden ser vacias"
zipMaximos [] _ = [] --aca deberia devolver un error??
zipMaximos _ [] = [] --aca deberia devolver un error??
--caso recursivo 
zipMaximos x y =  elMayorDe x : elMayorDe y : []

elMayorDe :: [Int] -> Int 
--dada una lista de numeros, devuelve el mayor 
--PRECOND: La lista no puede ser vacia 
--caso base 
elMayorDe [] = error "La lista no puede ser vacia"
elMayorDe (n:[]) = n --si solo hay un elemento, lo devuelvo   
--caso recursivo
elMayorDe (n:ns) = max n (elMayorDe ns) 
--15 
elMinimo :: Ord a => [a] -> a 
--caso base 
elMinimo [] = error "La lista no puede ser vacia"
elMinimo (x:[]) = x 
--caso recursivo
elMinimo (x:xs) = min x (elMinimo xs)

--2 recursion sobre numeros 
--1
factorial :: Int -> Int 
--precondicion: n no puede ser negativo
--caso base 
factorial 0 = 1
--caso recursivo
factorial n = n * factorial(n-1)
--2 
cuentaRegresiva :: Int -> [Int]
--caso base 
cuentaRegresiva 0 = []
--caso recursivo 
cuentaRegresiva n = n : cuentaRegresiva (n-1)
--3 
repetir :: Int -> a -> [a]
--caso base 
repetir 0 x = []
--caso recursivo 
repetir x a = a : repetir (x-1) a
--4 
losPrimeros :: Int -> [a] -> [a]
--caso base 
losPrimeros _ [] = []
--caso rescursivo
losPrimeros n (x:xs) = if n > 0
                        then x : losPrimeros (n-1) xs 
                            else losPrimeros (n-1) xs 
--5
sinLosPrimeros :: Int -> [a] -> [a]
--caso base 
sinLosPrimeros 0 a = a
--caso recursivo 
sinLosPrimeros n (x:xs) = if n /= 0 
                            then sinLosPrimeros (n-1) xs 
                                else x : sinLosPrimeros (n-1) xs 

--3 registros 
data Persona = P String Int --nombre edad 
    deriving Show 
--personas 
juan = P "Juan" 35 
maria = P "Maria" 26 
carlos = P "Carlos" 80
--observadoras 
edad :: Persona -> Int 
edad (P n e) = e
--1
mayoresA :: Int -> [Persona] -> [Persona]
--caso base 
mayoresA _ [] = []
--caso recursivo 
mayoresA x (p:ps) = if edad p > x 
                        then p : mayoresA x ps
                            else mayoresA x ps 
--2 
promedioEdad :: [Persona] -> Int 
--precondicion: la lista no puede ser vacia 
--caso base 
promedioEdad [] = error"La lista no puede ser vacia"
--caso recursivo (no es recursivo jeje se encarga la subtarea)
promedioEdad p = div (totalEdades p) (longitud p)

totalEdades :: [Persona] -> Int 
--dada una lista de personas devuelve el total de la suma de sus edades 
--caso base 
totalEdades [] = 0 
--caso recursivo 
totalEdades (p:ps) = edad p + totalEdades ps

elMasViejo :: [Persona] -> Persona 
--precondicion: la lista no es vacia 
--caso base 
elMasViejo [] = error"La lista no puede ser vacia"
elMasViejo (p:[]) = p
--caso recursivo
elMasViejo (x:xs) = elMasViejoEntre x (elMasViejo xs) 

elMasViejoEntre :: Persona -> Persona -> Persona 
--dadas dos personas, devuelve la mas vieja 
elMasViejoEntre x y = if edad x > edad y 
                        then x 
                            else y 
--3.2
data TipoDePokemon = Agua | Fuego | Planta
    deriving Show 
data Pokemon = ConsPokemon TipoDePokemon Int 
    deriving Show 
data Entrenador = ConsEntrenador String [Pokemon]
    deriving Show 

--pokemons de prueba 
charmander = ConsPokemon Fuego 100
squirtle = ConsPokemon Agua 75 
bulbasaur = ConsPokemon Planta 50

--Entrenadores 
ash = ConsEntrenador "Ash" [charmander, squirtle] 
red = ConsEntrenador "Red" [squirtle, bulbasaur, charmander]

cantPokemon :: Entrenador -> Int 
cantPokemon (ConsEntrenador _ []) = 0
cantPokemon (ConsEntrenador _ lp) = longitud lp

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int 
cantPokemonDe tp (ConsEntrenador _ (p:ps)) = contarPorTipo tp ps

--cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int 

esMaestroPokemon :: Entrenador -> Bool 
esMaestroPokemon (ConsEntrenador _ ps) = tieneUnoDeCada ps 

tieneUnoDeCada :: [Pokemon] -> Bool 
--indica si en la lista hay al menos un pokemon de cada tipo 
tieneUnoDeCada ps = hayUnoDe Fuego ps && hayUnoDe Planta ps && hayUnoDe Agua ps

hayUnoDe :: TipoDePokemon -> [Pokemon] -> Bool
--indica si hay al menos un pokemon del tipo dado en la lista 
--caso base 
hayUnoDe tp [] = False 
--caso recursivo
hayUnoDe tp (p:ps) = if not (esDeTipo tp p) 
                        then hayUnoDe tp ps
                            else True 
--auxiliares
esDeTipo :: TipoDePokemon -> Pokemon -> Bool
--dado un TipoDePokemon y un Pokemon devuelve True si los tipos coinciden 
esDeTipo Fuego (ConsPokemon Fuego _) = True   
esDeTipo Planta (ConsPokemon Planta _) = True
esDeTipo Agua (ConsPokemon Agua _) = True
esDeTipo _ (ConsPokemon _ _) = False     

contarPorTipo :: TipoDePokemon -> [Pokemon] -> Int 
--dado un TipoDePokemon y una lista de Pokemon, devuelve la cantidad del TipoDePokemon que hay en la lista 
--caso base 
contarPorTipo tp [] = 0 
--caso recursivo 
contarPorTipo tp (p:ps) = if esDeTipo tp p 
                            then 1 + contarPorTipo tp ps 
                                else contarPorTipo tp ps 
--3 
data Seniority = Junior | Semisenior | Senior 
    deriving Show 
data Proyecto = ConsProyecto String 
    deriving (Show, Eq) 
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show 
data Empresa = ConsEmpresa [Rol]
    deriving Show 

--set de datos 
--proyectos
sistDePagos = ConsProyecto "Sistema de pagos"
appMovil = ConsProyecto "App movil"
pagWeb = ConsProyecto "Pagina web"
iaService = ConsProyecto "Servicio de IA"
--empleados
--devs 
dev1 = Developer Junior sistDePagos
dev2 = Developer Senior appMovil
dev3 = Developer Semisenior sistDePagos
dev4 = Developer Junior iaService 
dev5 = Developer Senior pagWeb
--management 
mng1 = Management Senior iaService 
mng2 = Management Semisenior appMovil 
--empresas 
empresaVaga = ConsEmpresa [] --vacia
empresaSimple = ConsEmpresa [dev1, dev2, mng1] --todos los proyectos son distintos 
empresaGrande= ConsEmpresa [dev1, dev2, dev3, dev4, dev5, mng1, mng2] --empresa con los dos primeros proyectos repetidos 



proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa []) = []
proyectos (ConsEmpresa rs) = sinRepetidos (proyectosDe rs) 

proyectosDe :: [Rol] -> [Proyecto]
--dada una lista de roles, devuelve la lista de proyectos en los cuales los empleados trabajan 
--caso base 
proyectosDe [] = []
--caso recursivo 
proyectosDe (r:rs) = proyectoDe r : proyectosDe rs 

losDevSenior :: Empresa -> [Proyecto] -> Int 
losDevSenior (ConsEmpresa rs) lp = cantDeDevsSenior rs lp 

cantDeDevsSenior :: [Rol] -> [Proyecto] -> Int 
--dada una lista de roles y una lista de proyectos, devuelve la cant de devs senior de esa empresa que esten 
--trabajando en algun proyecto de la lista
--caso base 
cantDeDevsSenior [] _ = 0
cantDeDevsSenior _ [] = 0 
--caso recursivo
cantDeDevsSenior (r:rs) lp = if esDevSenior r && trabajaEn r lp 
                                then 1 + cantDeDevsSenior rs lp 
                                    else cantDeDevsSenior rs lp

esDevSenior :: Rol -> Bool 
--dado un rol, indica si es un dev senior 
esDevSenior (Developer Senior _) = True 
esDevSenior (Developer _ _) = False 
esDevSenior (Management _ _) = False 

trabajaEn :: Rol -> [Proyecto] -> Bool 
trabajaEn (Developer _ p) lp = pertenece p lp 
trabajaEn (Management _ p) lp = pertenece p lp 

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int 
cantQueTrabajanEn lp (ConsEmpresa lr) = cantQueTrabajan lp lr

cantQueTrabajan :: [Proyecto] -> [Rol] -> Int 
--dada una lista de proyectos y una de roles, indica la cantidad de empleados que trabajan en alguno de los
--proyectos de la lista
--caso base 
cantQueTrabajan [] _ = 0
cantQueTrabajan _[] = 0
--caso recursivo
cantQueTrabajan lp (r:rs) = if trabajaEn r lp 
                                then 1 + cantQueTrabajan lp rs 
                                    else cantQueTrabajan lp rs 

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = rolesAsignadosPorProyecto rs
--una empresa es una lista de roles, le delego la recursion a una funcion que maneje [Rol]

rolesAsignadosPorProyecto :: [Rol] -> [(Proyecto,Int)]
--caso base 
rolesAsignadosPorProyecto [] = []
--recursion sobre listas
rolesAsignadosPorProyecto (r:rs) = agregarATupla (proyectoDe r) (rolesAsignadosPorProyecto rs)
--aca extraigo el proyecto de r para usarlo como argumento en la funcion que arma las tuplas
--como es una recursion sobre listas, solo tengo que agregarlo, de eso se encarga la subtarea

agregarATupla :: Proyecto -> [(Proyecto, Int)] -> [(Proyecto, Int)]
--caso base 
agregarATupla p [] = (p,1) : []
--recursion sobre listas 
agregarATupla p ((q,n):qns) = if p == q 
                                then (q,n+1) : qns
                                    else (q,n) : agregarATupla p qns 
--aca accedo a la lista de tuplas y comparo proyectos, si esta en la lista de tuplas 
--solo le sumo 1 a la cantidad, si no dejo la primer tupla y sigo recorriendo
--si el proyecto no esta, llegara al caso base que se encarga de agregarlo



--observadora 
proyectoDe :: Rol -> Proyecto
proyectoDe (Developer _ p) = p
proyectoDe (Management _ p) = p 

--funcion para eliminar repetidos 
sinRepetidos :: Eq a => [a] -> [a]
--dada una lista, devuelve la misma sin repetidos 
--caso base 
sinRepetidos [] = []
--caso recursivo
sinRepetidos (a:as) = if pertenece a (sinRepetidos as)
                        then sinRepetidos as 
                            else a : sinRepetidos as 



{-siempre en la recursion separo la cabeza de lista y trabajo con eso, entonces solo me queda saber que hago con el 
resto de la lista, (que generalmente es aplicar la misma funcion, asi si funciona la logica de la cabeza de lista
va a funcionar para el resto; otra cosa es darse cuenta donde usar la recursion, ya que esta puede ser realizada 
por subtareas, tambien ver todos los casos base posibles (despues de hacer o plantear el recursivo)-}