module GestorDeTareas where

import System.IO

-- Sebastián Almánzar #10154600
-- 1. Tipo de Dato a presentar
data Tarea = Tarea Int String

-- 2. Funciones Puras (clave del programa, se muestra tarea, se obtiene su ID, se agrega tarea, eliminar tarea por su ID.

-- La tarea pasa de un ID (Int) a un String
visualizarTarea :: Tarea -> String
visualizarTarea (Tarea id desc) = "ID: " ++ show id ++ " - " ++ desc

-- Se obtiene un ID
obtenerIdDeTarea :: Tarea -> Int
obtenerIdDeTarea (Tarea id _) = id

-- Se genera un nuevo ID
siguienteId :: [Tarea] -> Int
siguienteId [] = 1
siguienteId tareas = obtenerIdDeTarea (last tareas) + 1

-- El usuario puede agregar una nueva tarea a su Lista
agregarTarea :: [Tarea] -> String -> [Tarea]
agregarTarea tareas descripcion =
  let nuevoID = siguienteId tareas
      nuevaTarea = Tarea nuevoID descripcion
   in tareas ++ [nuevaTarea]

-- Eliminar una tarea por su ID
eliminarTarea :: [Tarea] -> Int -> [Tarea]
eliminarTarea tareas idEliminar =
  filter (\t -> obtenerIdDeTarea t /= idEliminar) tareas

main :: IO ()
main = do
  let listaInicial = []
  let lista1 = agregarTarea listaInicial "Tengo que realizar Practica-3 de Programacion Web"
  let lista2 = agregarTarea lista1 "Pagar la luz"
  let lista3 = agregarTarea lista2 "Estudiar Haskell"

  putStrLn "-----Mis Tareas -----"
  mapM_ (putStrLn . visualizarTarea) lista2

  putStrLn "\n--- Eliminando Tarea 1 ---"
  let listaFinal = eliminarTarea lista2 1
  mapM_ (putStrLn . visualizarTarea) listaFinal
