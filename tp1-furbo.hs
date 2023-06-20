--potrero
data Equipo = Equipo{
    nombre :: String,
    canVict :: Int,
    cantJugadores :: [Jugador]
} deriving Show
data Jugador = Jugador{
    nomJug :: String,
    valoracionBase :: Float
} deriving Show
data Bebida = Bebida{
    nomBebida :: String,
    porcentajeAlteracion :: Float
}deriving Show
bauti :: Jugador
bauti = Jugador{
    nomJug = "bauti",
    valoracionBase = 100
}
gabi :: Jugador
gabi = Jugador{
    nomJug = "Gabi",
    valoracionBase = 100
}
fabri :: Jugador
fabri = Jugador{
    nomJug = "Fabri",
    valoracionBase = 100
}
cerveza :: Bebida
cerveza = Bebida{
    nomBebida = "Cerveza fuerte",
    porcentajeAlteracion = -0.15
}
agua :: Bebida
agua = Bebida{
    nomBebida = "agua",
    porcentajeAlteracion = 0.0
}
coca :: Bebida
coca = Bebida{
    nomBebida = "coca",
    porcentajeAlteracion = 0.52
}
nuevaValoracion :: Bebida -> Jugador -> Jugador
nuevaValoracion (Bebida _ porcentajeAlteracion) (Jugador nomJug valoracionBase) = (Jugador nomJug (valoracionBase + (valoracionBase * porcentajeAlteracion)) )
--despues se sigue, ahora paso

equipoWin :: Equipo -> Equipo -> Bool
equipoWin 

--la libertadores !

data EquipoLib = EquipoLib{
    nombreLib :: String,
    canVictLib :: Int,
    cantJugadoresLib :: [Jugador],
    dt :: Dt
} deriving Show

data Dt = Dt {
    nombreDt :: String,
    estilo :: Estilo
}





dtLib :: Estilo -> EquipoLib ->EquipoLib
dtLib nuevoEstilo (EquipoLib nombreLib canVictLib cantJugadoresLib estilodt) = (EquipoLib nombreLib canVictLib cantJugadoresLib nuevoEstilo)