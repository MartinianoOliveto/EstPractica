--Numeros enteros 
--a
sucesor :: Int -> Int 
sucesor n = n + 1 
--b
sumar :: Int -> Int -> Int 
sumar n m = n + m 
--c
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = ((div n m), (mod n m))
--d 
maxDelPar :: (Int, Int) -> Int 
maxDelPar (n, m) = if n >= m 
                    then n 
                        else m 
--2

--tipos enumerativos
data Dir = Norte | Este | Sur | Oeste  
    deriving Show 
--a
opuesto :: Dir -> Dir 
opuesto Norte = Sur 
opuesto Este = Oeste 
opuesto Sur = Norte 
opuesto Oeste = Este 
--b
iguales :: Dir -> Dir -> Bool
iguales Este Este = True 
iguales Norte Norte = True 
iguales Sur Sur = True 
iguales Oeste Oeste = True 
iguales _ _= False 
--c 
siguiente :: Dir -> Dir 
-- no posee precondicion, es una funcion total, ya que solo se le puede pasar direcciones 
siguiente Norte = Este 
siguiente Este = Sur 
siguiente Sur = Oeste 
siguiente Oeste = Norte  
--2
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo 
    deriving Show 
--a
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDiaSemana, ultimoDiaSemana)

primerDiaSemana :: DiaDeSemana
primerDiaSemana = Lunes 
ultimoDiaSemana :: DiaDeSemana
ultimoDiaSemana = Domingo 
--b
empiezaConM :: DiaDeSemana -> Bool 
empiezaConM Martes = True 
empiezaConM Miercoles = True 
empiezaConM _ = False 
--c 
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool 
vieneDespues d1 d2 = valor d1 > valor d2 

valor :: DiaDeSemana -> Int 
valor Lunes = 1 
valor Martes = 2 
valor Miercoles = 3 
valor Jueves = 4 
valor Viernes = 5 
valor Sabado = 6 
valor Domingo = 7 
--tratar de resolver usando las subtareas definidas en a; y sin asignar numeros a los dias de la semana
--d
estaEnElMedio :: DiaDeSemana -> Bool 
estaEnElMedio Lunes = False 
estaEnElMedio Domingo = False 
estaEnElMedio _ = True 
{-estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio primerDiaSemana = False 
estaEnElMedio ultimoDiaSemana = False 
-- en estas dos el PM es redundante?? 
estaEnElMedio _ = True 
SIEMPRE DA FALSO??-}
--3
--a
negar :: Bool -> Bool 
negar True = False 
negar False = True 
--b 
implica :: Bool -> Bool -> Bool 
implica True b = not b 
implica False _= False 
--preguntar si esta bien 
--c 
yTambien :: Bool -> Bool -> Bool 
yTambien b b' = if b == b'
                    then True 
                        else False
--d 
oBien :: Bool -> Bool -> Bool 
oBien True _ = True 
oBien _ True = True 
oBien _ _ = False

--registros
data Persona = P String Int --nombre edad 
    deriving Show
--personas de prueba 
juan = P "Juan" 35 
maria = P "Maria" 26 
--a
nombre :: Persona -> String 
nombre (P n e) = n
--b 
edad :: Persona -> Int 
edad (P n e) = e
--c 
crecer :: Persona -> Persona 
crecer (P n e) = (P n(sucesor e))
--d 
cambioDeNombre :: String -> Persona -> Persona 
cambioDeNombre s (P n e) = (P s e)
--e 
esMayorQueLaOtra :: Persona -> Persona -> Bool 
esMayorQueLaOtra p1 p2 = if edad p1 > edad p2 
                            then True 
                                else False
--f
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if edad p1 > edad p2 
                        then p1 
                            else p2 
--2
data Pokemon = P' TipoDePokemon Int --tipoDePokemon energia
    deriving Show 
data TipoDePokemon = Agua | Fuego | Planta 
    deriving Show 
data Entrenador = E String Pokemon Pokemon --nombre tipoDePokemon tipoDePokemon
    deriving Show 
--Observadoras 
tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (P' t e) = t 
--pokemons de prueba 
charmander = P' Fuego 100
squirtle = P' Agua 75 
bulbasaur = P' Planta 50
--Entrenadores 
ash = E "Ash" charmander squirtle 
red = E "Red" squirtle bulbasaur
--a
superaA :: Pokemon -> Pokemon -> Bool 
superaA p1 p2 = if elementoSuperior (tipoDePokemon p1) (tipoDePokemon p2)
                    then True 
                        else False
elementoSuperior :: TipoDePokemon -> TipoDePokemon -> Bool 
--dados dos elementos indica si el primero supera al segundo 
elementoSuperior Agua Fuego = True 
elementoSuperior Fuego Planta = True 
elementoSuperior Planta Agua = True 
elementoSuperior _ _ = False 
--b
--cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int 
--c 
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = pokemonsDe e1 ++ pokemonsDe e2
pokemonsDe :: Entrenador -> [Pokemon]
pokemonsDe (E n p1 p2) = [p1,p2]