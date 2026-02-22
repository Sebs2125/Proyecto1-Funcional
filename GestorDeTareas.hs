module GestorDeTareas where

import System.IO

-- Esmil Echavarria 10154964
-- Sebastián Almánzar 10154600
-- Ariel Diaz 10153927

-- 1. Tipo de Dato a presentar:
-- Int -> se encarga de almacenar los ID de las tareas.
-- String -> se encarga de almacenar las descripciones de las tareas.
-- Bool -> se encarga de almacenar el estado de las tareas (si fue completada o no).
data Tarea = Tarea Int String Bool

-- 2. Funciones Puras (clave del programa, se muestra tarea, se obtiene su ID, se agrega tarea, eliminar tarea por su ID:

-- La tarea pasa de un ID (Int) a un String
-- Ejemplo: ID: 1 - Descripción: Completar el Proyecto número 1 de Programación Funcional - Estado: Pendiente.
visualizarTarea :: Tarea -> String
visualizarTarea (Tarea id desc comp) = "ID: " ++ show id ++ " - " ++ desc ++ " - " ++ show comp

-- Se obtiene un ID, simplemente obtiene el ID de una tarea en específico que se necesite.
-- Por ejemplo el ID: 1 del ejemplo en visualizarTarea (le va a mostrar directamente el contenido que posee, lo importante que desea el usuario es verificar su descripción y estado).
obtenerIdDeTarea :: Tarea -> Int
obtenerIdDeTarea (Tarea id _ _) = id

-- 3. Uso de Recursividad:

-- Se genera un nuevo ID, se encarga de generar un nuevo ID para la tarea que se va a agregar.
-- Simplemente de una lista vacía, si es la primera tarea del usuario (+1 = 1, si añade otra (+1 = 2, itera todo el tiempo) ).
siguienteId :: [Tarea] -> Int
siguienteId [] = 1
siguienteId tareas = obtenerIdDeTarea (last tareas) + 1

-- El usuario puede agregar una nueva tarea a su Lista
-- Al añadir una nueva tarea, su ID se suma (+1) a la última tarea agregada y la lista se agranda y se actualiza automáticamente.
agregarTarea :: [Tarea] -> String -> [Tarea]
agregarTarea tareas descripcion =
  let nuevoID = siguienteId tareas
      nuevaTarea = Tarea nuevoID descripcion False
   in tareas ++ [nuevaTarea]

-- 4. Uso de Pattern Matching:

-- Cambiar el Bool de una tarea a true
-- Recorre la lista de tareas mediante un map (uno por uno), si el ID coincide con el ID retorna True. Si no coincide retorna la tarea igualmente (pero dejando saber que no está completada)
completarTarea :: [Tarea] -> Int -> [Tarea]
completarTarea tareas idTarget =
  map
    ( \(Tarea id desc comp) ->
        if id == idTarget
          then Tarea id desc True
          else Tarea id desc comp
    )
    tareas

-- 3. Uso de Recursividad denuevo (se utiliza mejor que en siguienteId):

-- Eliminar una tarea por su ID
-- Si la lista está vacía retorna la lista vacía.
-- Si coincide los datos se elimina la tarea
-- Si no coincide se mantiene la tarea
eliminarTarea :: [Tarea] -> Int -> [Tarea]
eliminarTarea [] _ = []
eliminarTarea (x : xs) elim
  | obtenerIdDeTarea x == elim = xs -- Si coincide los datos se elimina la tarea
  | otherwise = x : eliminarTarea xs elim -- Si no coincide se mantiene la tarea

-- 5. Manejo de Errores con Maybe:

-- Funcion para leer entero
-- La funcion se encarga de retornar un String a Int, manejado por errores. Si es Nothing -> Error, si es Just -> Valor
leerentero :: String -> Maybe Int
leerentero s = case reads s of -- Utiliza 'reads' para  verificar que sea numero.
  [(val, "")] -> Just val
  _ -> Nothing

-- 6. Interacción Mínima con IO:

main :: IO ()
main = menu []

menu :: [Tarea] -> IO ()
menu tareas = do
  putStrLn "\n========== GESTOR DE TAREAS =========="
  -- Mostramos la lista actual
  if null tareas -- Si no hay tareas, devuelve la lista vacía.
    then putStrLn "[ Lista vacia ]"
    else mapM_ (putStrLn . visualizarTarea) tareas

  putStrLn "\n1. Agregar Tarea" -- Empieza el menú del proyecto donde el usuario puede libremente usar cualquier opción para sus necesidades.
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
