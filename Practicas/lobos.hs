
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