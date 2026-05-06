
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
  deriving Show
data Manada = M Lobo
  deriving Show


-- 1. Construir un valor de tip o Manada que posea 1 cazador, 2 exploradores y que el resto sean crías. Resolver las siguientes funciones utilizando recursión estructural sobre la estructura que corresp onda en cada caso:
manada1 =M(Cazador "Akela" ["ciervo","conejo"]
            (Explorador "Raksha" ["Norte"]
              (Cria "Luna")
              (Cria "Rayo")
            )
            (Explorador "Kiba" ["Sur","Este"]
              (Cria "Sombra")
              (Cria "Nube")
            )
            (Cria "Chispa")
        )

manada2 = M (Cazador "" []
              (Explorador .. ...
                (Cria ...)
                (Cria ...)
              )
              (Explorador ... ... 
                (Cria ....)
                (Cria ....)
              )
              (Cria ...)
            )

data Objeto = Tesoro | Chatarra
  deriving Show 
data Cofre = Cofre [Objeto]
  deriving Show 
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
  deriving Show 

caminoAlTesoro :: Mapa -> [Dir]
--hay un tesoro y es unico 
(Fin c) = [] 
(Bifurcacion c t1 t2) = if hayTesoroEnCofre c
                        then []
                        else if hayTesoro t1
                            then Izq : caminoAlTesoro t1
                            else Der : caminoAlTesoro t2 
