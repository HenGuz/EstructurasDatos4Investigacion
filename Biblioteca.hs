import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO (hFlush, stdout)


type Libro = String
type Usuario = String
type Fecha = String


data Biblioteca = Biblioteca {
    disponibles :: Set.Set Libro,
    prestados   :: Map.Map Usuario (Libro, Fecha),
    historial   :: [(Usuario, Libro, Fecha)],
    morosos     :: Set.Set Usuario
} deriving (Show)


bibliotecaInicial :: Biblioteca
bibliotecaInicial = Biblioteca {
    disponibles = Set.fromList ["Haskell", "Python", "Java", "C++", "Rust", "JavaScript"],
    prestados   = Map.empty,
    historial   = [],
    morosos     = Set.empty
}


prestarLibro :: Usuario -> Libro -> Fecha -> Biblioteca -> Either String Biblioteca
prestarLibro usuario libro fecha bib
    | usuario `Set.member` morosos bib = 
        Left $ "\n Usuario " ++ usuario ++ " no puede pedir préstamos."
    | not (libro `Set.member` disponibles bib) =
        Left $ "\n El libro '" ++ libro ++ "' no está disponible."
    | otherwise = Right $ bib {
        disponibles = Set.delete libro (disponibles bib),
        prestados   = Map.insert usuario (libro, fecha) (prestados bib),
        historial   = (usuario, libro, fecha) : historial bib
    }


devolverLibro :: Usuario -> Fecha -> Biblioteca -> Either String Biblioteca
devolverLibro usuario fechaActual bib =
    case Map.lookup usuario (prestados bib) of
        Nothing -> Left $ "\n Usuario " ++ usuario ++ " no tiene préstamos activos."
        Just (libro, fechaPrestamo) ->
            Right $ bib {
                disponibles = Set.insert libro (disponibles bib),
                prestados   = Map.delete usuario (prestados bib),
                historial   = (usuario, libro, fechaActual) : historial bib
            }


marcarMoroso :: Usuario -> Biblioteca -> Biblioteca
marcarMoroso usuario bib = bib {
    morosos = Set.insert usuario (morosos bib)
}


mostrarDisponibles :: Biblioteca -> IO ()
mostrarDisponibles bib = do
    putStrLn "\n Libros disponibles:"
    if Set.null (disponibles bib)
        then putStrLn "  No hay libros disponibles"
        else mapM_ (\libro -> putStrLn $ "  • " ++ libro) (Set.toList (disponibles bib))


mostrarPrestamos :: Biblioteca -> IO ()
mostrarPrestamos bib = do
    putStrLn "\n Préstamos activos:"
    if Map.null (prestados bib)
        then putStrLn "  No hay préstamos activos"
        else mapM_ (\(usr, (lib, fecha)) -> 
            putStrLn $ "  • " ++ usr ++ " → '" ++ lib ++ "' (desde: " ++ fecha ++ ")")
            (Map.toList (prestados bib))


mostrarMorosos :: Biblioteca -> IO ()
mostrarMorosos bib = do
    putStrLn "\n Usuarios con mora:"
    if Set.null (morosos bib)
        then putStrLn "  No hay usuarios morosos"
        else mapM_ (\usr -> putStrLn $ "  • " ++ usr) (Set.toList (morosos bib))


mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "\n" ++ replicate 40 '='
    putStrLn "   Sistema de Biblioteca"
    putStrLn (replicate 40 '=')
    putStrLn "1. Prestar libro"
    putStrLn "2. Devolver libro"
    putStrLn "3. Ver libros disponibles"
    putStrLn "4. Ver préstamos activos"
    putStrLn "5. Ver usuarios morosos"
    putStrLn "6. Marcar usuario como moroso"
    putStrLn "7. Ver historial completo"
    putStrLn "0. Salir"
    putStr (replicate 40 '-')
    putStr "\nOpción: "
    hFlush stdout


leerOpcion :: IO String
leerOpcion = getLine


interactuarPrestar :: Biblioteca -> IO (Either String Biblioteca)
interactuarPrestar bib = do
    putStr "\n Nombre del usuario: "
    hFlush stdout
    usuario <- getLine
    putStr " Nombre del libro: "
    hFlush stdout
    libro <- getLine
    putStr " Fecha (YYYY-MM-DD): "
    hFlush stdout
    fecha <- getLine
    return $ prestarLibro usuario libro fecha bib


interactuarDevolver :: Biblioteca -> IO (Either String Biblioteca)
interactuarDevolver bib = do
    putStr "\n Nombre del usuario: "
    hFlush stdout
    usuario <- getLine
    putStr " Fecha de devolución (YYYY-MM-DD): "
    hFlush stdout
    fecha <- getLine
    return $ devolverLibro usuario fecha bib


interactuarMarcarMoroso :: Biblioteca -> IO Biblioteca
interactuarMarcarMoroso bib = do
    putStr "\n Usuario a marcar como moroso: "
    hFlush stdout
    usuario <- getLine
    let nuevaBib = marcarMoroso usuario bib
    putStrLn $ "\n" ++ usuario ++ " ha sido marcado como moroso"
    return nuevaBib


mostrarHistorial :: Biblioteca -> IO ()
mostrarHistorial bib = do
    putStrLn "\n HISTORIAL COMPLETO DE PRÉSTAMOS:"
    if null (historial bib)
        then putStrLn "  No hay préstamos registrados"
        else mapM_ (\(usr, lib, fecha) -> 
            putStrLn $ "  • " ++ usr ++ " → '" ++ lib ++ "' (" ++ fecha ++ ")")
            (reverse (historial bib))


main :: IO ()
main = do
    putStrLn "\n Bienvenido al sistema de la biblioteca"
    loop bibliotecaInicial
    where
        loop :: Biblioteca -> IO ()
        loop bib = do
            mostrarMenu
            opcion <- leerOpcion
            
            case opcion of
                "1" -> do
                    resultado <- interactuarPrestar bib
                    case resultado of
                        Left err -> do
                            putStrLn err
                            loop bib
                        Right nuevaBib -> do
                            putStrLn "\n Préstamo realizado con éxito"
                            loop nuevaBib
                
                "2" -> do
                    resultado <- interactuarDevolver bib
                    case resultado of
                        Left err -> do
                            putStrLn err
                            loop bib
                        Right nuevaBib -> do
                            putStrLn "\n Devolución registrada"
                            loop nuevaBib
                
                "3" -> do
                    mostrarDisponibles bib
                    loop bib
                
                "4" -> do
                    mostrarPrestamos bib
                    loop bib
                
                "5" -> do
                    mostrarMorosos bib
                    loop bib
                
                "6" -> do
                    nuevaBib <- interactuarMarcarMoroso bib
                    loop nuevaBib
                
                "7" -> do
                    mostrarHistorial bib
                    loop bib
                
                "0" -> do
                    putStrLn "\n Gracias por usar el sistema"
                    putStrLn "  Adios\n"
                    return ()
                
                _ -> do
                    putStrLn "\n Opción inválida. Intenta de nuevo."
                    loop bib
