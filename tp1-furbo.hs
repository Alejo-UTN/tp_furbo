data Equipo = Equipo
  { nombre :: String,
    jugadoresRendimiento :: [Float],
    estiloJuego :: String
  }
  deriving (Show)

data Jugador = Jugador
  { nombreJugador :: String,
    rendimiento :: Float
  }
  deriving (Show)

-- Primero se preparar los jugadores antes de cualquier partido y/o torneo
prepararJugador :: String -> String -> Float -> Jugador
prepararJugador nombre bebidaPrevia rendimientoActual = Jugador nombre (calcularRendimiento rendimientoActual bebidaPrevia)

calcularRendimiento :: Float -> String -> Float
calcularRendimiento rendimientoActual incentivo = rendimientoActual + (rendimientoActual * incentivos incentivo) / 100.0

incentivos :: String -> Float
incentivos "cafe" = 5.0
incentivos "cerveza" = -10.0
incentivos _ = 0

-- Luego se fichan los jugadores en el equipo que uno desee
ficharJugadores :: [Jugador] -> String -> String -> Equipo
ficharJugadores jugadores nombre estiloJuego = Equipo nombre (map rendimiento jugadores) estiloJuego

-- El rendimiento depende del estilo de juego
calcularRendimientoEquipo :: Equipo -> Float
calcularRendimientoEquipo equipo
  | estiloJuego equipo == "Potrero" = sum (take 5 (jugadoresRendimiento equipo)) / 5 -- Rendimiento promedio de los 5 jugadores
  | estiloJuego equipo == "Elitista" = maximum (jugadoresRendimiento equipo) -- El rendimiento del equipo es el del mejor jugador
  | estiloJuego equipo == "Doping" = (sum (take 5 (jugadoresRendimiento equipo)) / 5) * 2 -- Se duplica el rendimiento de los 5 jugadores
  | otherwise = sum (take 3 (jugadoresRendimiento equipo)) / 3 -- Rendimiento para potrero default

-- si un equipo tiene más rendimiento que el otro ganá. En caso de empate, gana el visitante (Eq2)
partido :: (Equipo, Equipo) -> Equipo
partido (eq1, eq2)
  | calcularRendimientoEquipo eq1 > calcularRendimientoEquipo eq2 = eq1
  | otherwise = eq2

-- Devuelve el equipo que haya ganado todos los partidos
torneo :: [Equipo] -> Equipo
torneo [ganador] = ganador
torneo equipos = torneo (enfrentamientos equipos)

enfrentamientos :: [Equipo] -> [Equipo]
-- Si hay enfrentamientos vacios o con un solo equipo devuelve lo mismo
enfrentamientos [] = []
enfrentamientos [equipo] = [equipo]
-- En caso de que hayan mas de dos equipos, hace partidos por pares y el ganador se queda en la lista (Se vuelve a llamar a la funcion de forma recursiva)
enfrentamientos (equipo1 : equipo2 : resto) = partido (equipo1, equipo2) : enfrentamientos resto

libertadores :: [Equipo] -> Equipo
libertadores [ganador] = ganador
libertadores equipos = libertadores (partidosLlaves (agruparPartidos equipos))

partidosLlaves :: [(Equipo, Equipo)] -> [Equipo]
partidosLlaves [] = []
partidosLlaves ((equipo1, equipo2) : resto) = partido (equipo1, equipo2) : partidosLlaves resto

-- Se agrupan los equipos para formar las distintas llaves del campeonato (Si son impar, un equipo se queda descalificado)
agruparPartidos :: [Equipo] -> [(Equipo, Equipo)]
agruparPartidos [] = []
agruparPartidos [_] = []
agruparPartidos (equipo1 : equipo2 : resto) = (equipo1, equipo2) : agruparPartidos resto