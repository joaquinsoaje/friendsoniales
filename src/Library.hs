module Library where
import PdePreludat

-- Constants
muchasTareas = 5
edadLimiteJoven = 40
cantidadCriticados = 2
cuantoRelajaHacerTarea = -10

type CriterioEnergia = Persona -> Bool
type Criterio = Persona -> Bool

data Persona = Persona {
    nombre :: String,
    edad :: Number,
    alegria :: Number,
    ansiedad :: Number,
    cantidadTareas :: Number
} deriving (Eq, Show, Ord)

-- -- -- -- -- -- -- -- -- Punto 1 -- -- -- -- -- -- -- -- -- --
nivelEnergia :: Persona->Number
nivelEnergia persona
    | esMasAlegre persona = min 340.(2*).alegria $ persona
    | esAnsiosoJoven persona = (-300+).nivelEstres $ persona
    | otherwise = (+ 10).alegria $ persona

nivelEstres :: Persona -> Number
nivelEstres persona
    | muchasTareasPendientes persona = (*1.5).ansiedad $ persona
    | otherwise = ansiedad persona

muchasTareasPendientes :: Criterio
muchasTareasPendientes = (> muchasTareas).cantidadTareas

esMasAlegre :: CriterioEnergia
esMasAlegre persona = (> ansiedad persona).alegria $ persona

esMasAnsioso :: CriterioEnergia
esMasAnsioso persona = (> ansiedad persona).alegria $ persona

esAnsiosoJoven :: CriterioEnergia -- Es un joako
esAnsiosoJoven persona = esJoven persona && esMasAnsioso persona

esJoven :: CriterioEnergia
esJoven = (<edadLimiteJoven).edad

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- -- -- -- -- -- -- -- -- Punto 2 -- -- -- -- -- -- -- -- -- --

cuantoDueleVerLasBuenas :: [Persona]->Bool
cuantoDueleVerLasBuenas = sonVitales.obtenerJovenes

obtenerJovenes :: [Persona] -> [Persona]
obtenerJovenes = filter esJoven

sonVitales :: [Persona] -> Bool
sonVitales = all ((>100).nivelEnergia)


nivelTotalDeAnsiedad :: [Persona]->Number
nivelTotalDeAnsiedad = sum.map ansiedad.obtenerJovenes


losMasCriticados :: Criterio->[Persona]->[String]

losMasCriticados criterio = map nombre.take cantidadCriticados.filter criterio


-- Ejemplos puestos en los tests, sino acá hay ejemplo
-- joako = Persona{ nombre = "Joako", edad = 32, alegria = 0, ansiedad = 999, tareas = 1234 }
-- ayudante = Persona{ nombre = "Solo Ayudante", edad = 79, alegria = 10, ansiedad = 9, tareas = 4321 }
-- losMasCriticados ((>50).ansiedad) [joako, ayudante]
-- losMasCriticados (even.nivelEnergia) [ayudante, joako]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- -- -- -- -- -- -- -- -- Punto 3 -- -- -- -- -- -- -- -- -- --

type Tarea = Persona -> Persona

codearUnProyectoNuevo :: Tarea
hacerTramitesEnAfip :: Number -> Tarea
andarEnBici :: Number -> Tarea
escucharMusica :: Tarea

tareaGenerico :: Persona -> Persona
tareaGenerico persona = persona {
    ansiedad = max 0 . (cuantoRelajaHacerTarea+) . ansiedad $ persona,
    cantidadTareas = max 0 . (-1+) . cantidadTareas $ persona
}

codearUnProyectoNuevo persona = tareaGenerico persona {
    alegria = (+ 110) . alegria $ persona,
    ansiedad = (+ 50) . ansiedad $ persona
}

hacerTramitesEnAfip cantidadTramites persona = tareaGenerico persona {
    ansiedad = max 300 . (cantidadTramites*) . ansiedad $ persona
}

andarEnBici kilometros persona = tareaGenerico persona {
    ansiedad = 0,
    alegria = (kilometros*50+) . alegria $ persona
}

escucharMusica persona = tareaGenerico persona {
    ansiedad = (-10+) . ansiedad $ persona
}

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- -- -- -- -- -- -- -- -- Punto 4 -- -- -- -- -- -- -- -- -- --
energiaResultante :: Persona -> [Tarea] -> Number
energiaResultante persona tareas = nivelEnergia (foldl (\p tarea -> tarea p) persona tareas)

hiceLoQuePude :: Persona -> [Tarea]->Persona
hiceLoQuePude persona [] = persona
hiceLoQuePude persona (tarea:tareas)
    | tareaDejaConEnergia tarea persona = hiceLoQuePude (tarea persona) tareas
    | otherwise = persona

tareaDejaConEnergia :: Tarea -> Persona -> Bool
tareaDejaConEnergia tarea = (>100).nivelEnergia.tarea
