module GestorDeTareas where

import System.IO

-- Esmil Echavarria 10154964
-- Sebastián Almánzar 10154600
-- Ariel Diaz 10153927

-- 1. Tipo de Dato a presentar
data Tarea = Tarea Int String Bool

-- 2. Funciones Puras (clave del programa, se muestra tarea, se obtiene su ID, se agrega tarea, eliminar tarea por su ID.

-- La tarea pasa de un ID (Int) a un String
visualizarTarea :: Tarea -> String
visualizarTarea (Tarea id desc comp) = "ID: " ++ show id ++ " - " ++ desc ++ " - " ++ show comp

-- Se obtiene un ID
obtenerIdDeTarea :: Tarea -> Int
obtenerIdDeTarea (Tarea id _ _) = id

-- Se genera un nuevo ID
siguienteId :: [Tarea] -> Int
siguienteId [] = 1
siguienteId tareas = obtenerIdDeTarea (last tareas) + 1

-- El usuario puede agregar una nueva tarea a su Lista
agregarTarea :: [Tarea] -> String -> [Tarea]
agregarTarea tareas descripcion =
  let nuevoID = siguienteId tareas
      nuevaTarea = Tarea nuevoID descripcion False
   in tareas ++ [nuevaTarea]

-- Cambiar el Bool de una tarea a true
completarTarea :: [Tarea] -> Int -> [Tarea]
completarTarea tareas idTarget =
  map
    ( \(Tarea id desc comp) ->
        if id == idTarget
          then Tarea id desc True
          else Tarea id desc comp
    )
    tareas

-- Eliminar una tarea por su ID
eliminarTarea :: [Tarea] -> Int -> [Tarea]
eliminarTarea [] _ = []
eliminarTarea (x : xs) elim
  | obtenerIdDeTarea x == elim = xs -- Si coincide los datos se elimina la tarea
  | otherwise = x : eliminarTarea xs elim -- Si no coincide se mantiene la tarea

-- Funcion para leer entero
leerentero :: String -> Maybe Int
leerentero s = case reads s of -- Utiliza 'reads' para  verificar que sea numero.
  [(val, "")] -> Just val
  _ -> Nothing

main :: IO ()
main = menu []

menu :: [Tarea] -> IO ()
menu tareas = do
  putStrLn "\n========== GESTOR DE TAREAS =========="
  -- Mostramos la lista actual
  if null tareas
    then putStrLn "[ Lista vacia ]"
    else mapM_ (putStrLn . visualizarTarea) tareas

  putStrLn "\n1. Agregar Tarea"
  putStrLn "2. Marcar como Completada"
  putStrLn "3. Ver solo Pendientes"
  putStrLn "4. Eliminar Tarea"
  putStrLn "5. Salir"
  putStr "Seleccione una opción: "
  hFlush stdout
  opcion <- getLine
  case opcion of
    "1" -> do
      putStr "Descripción de la tarea: "
      desc <- getLine
      let nuevasTareas = agregarTarea tareas desc
      menu nuevasTareas -- Llamada con la lista actualizada
    "2" -> do
      putStr "ID de la tarea a completar: "
      idStr <- getLine
      let n = maybe tareas (completarTarea tareas) (leerentero idStr)
      -- Esto es como if si es valido lo completa, si no, no hace nada
      menu n
    "3" -> do
      putStrLn "\n--- Tareas Pendientes ---"
      let pendientes = filter (\(Tarea _ _ comp) -> not comp) tareas
      mapM_ (putStrLn . visualizarTarea) pendientes
      putStrLn "Presione Enter para volver..."
      _ <- getLine
      menu tareas -- Volvemos al menú con todas las tareas
    "4" -> do
      putStr "ID de la tarea a eliminar: "
      idStr <- getLine
      let n = maybe tareas (eliminarTarea tareas) (leerentero idStr)
      menu n
    "5" -> putStrLn "Cerrando programa"
    _ -> do
      putStrLn "Opción no válida"
      menu tareas
