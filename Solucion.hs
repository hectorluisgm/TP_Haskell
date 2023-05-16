module Solucion where
-- main :: IO ()
-- main = putStrLn "Hola, mundo!"

-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios redSocial| redSocialValida redSocial == False = error "Red Social no cumple los requisitos"
                           | otherwise = proyectarNombres(usuarios(redSocial))

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos [] = True
noHayIdsRepetidos (x:xs) | pertenece (idDeUsuario x) (hacerLista xs) = False
                         | otherwise = noHayIdsRepetidos xs


hacerLista:: [Usuario] -> [Integer]
hacerLista [] = []
hacerLista (x:xs) = idDeUsuario (x) : hacerLista (xs)


pertenece :: Integer -> [Integer] -> Bool
pertenece a [] = False
pertenece a (x:xs) | a == x = True 
                   | otherwise = pertenece a xs

longitud :: [t] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

usuarioValido :: Usuario -> Bool
usuarioValido a  | idDeUsuario a > 0 && longitud (nombreDeUsuario a) > 0 = True
                 | otherwise = False


usuariosValidos :: [Usuario] -> Bool
usuariosValidos [] = True
usuariosValidos (x:xs) | noHayIdsRepetidos (x:xs) == True && usuarioValido x == True = usuariosValidos (xs)
                       | otherwise = False

hacerListaRelacion:: [Relacion] -> [(Integer, Integer)]
hacerListaRelacion [] = []
hacerListaRelacion (x:xs) = ((idDeUsuario(primerElemento (x)), (idDeUsuario(segundoElemento(x)))): hacerListaRelacion(xs))


perteneceUsuario :: Usuario -> [Usuario] -> Bool
perteneceUsuario a [] = False
perteneceUsuario a (x:xs) | a == x = True 
                          | otherwise = perteneceUsuario a xs

perteneceRelacion :: (Relacion) -> [Relacion] -> Bool
perteneceRelacion a [] = False
perteneceRelacion a (x:xs) | a == x = True 
                           | otherwise = perteneceRelacion a xs

perteneceTupla:: (Integer, Integer) -> [(Integer,Integer)] -> Bool
perteneceTupla a [] = False
perteneceTupla a (x:xs) | a == x = True 
                   | otherwise = perteneceTupla a xs

relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas [] (y:ys) = False
relacionesValidas (x:xs) [] = True
relacionesValidas (x:xs) (y:ys)|usuariosDeRelacionesValidos (x:xs) (y:ys) == True && relacionesAsimetricas (y:ys) == True && noHayRelacionesRepetidas (y:ys) == True = True
                               |otherwise = False
                               
usuariosDeRelacionesValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionesValidos (x:xs) [] = True
usuariosDeRelacionesValidos [] (y:ys) = False
usuariosDeRelacionesValidos (x:xs) (y:ys) | primerElemento (y) == segundoElemento (y) || perteneceUsuario (fst(y)) ((x:xs)) == False || perteneceUsuario (snd(y)) ((x:xs)) == False = False
                                          | otherwise = usuariosDeRelacionesValidos (x:xs) ys

relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas [] = True
relacionesAsimetricas (x:xs) | perteneceRelacion ((segundoElemento (x), primerElemento (x))) (x:xs) == True = False
                             |otherwise = relacionesAsimetricas xs

noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas [] = True
noHayRelacionesRepetidas (x:xs) | perteneceTupla (idDeUsuario (primerElemento (x)), idDeUsuario(segundoElemento(x))) (hacerListaRelacion xs) == True =False
                                | otherwise = noHayRelacionesRepetidas (xs)

usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed [] (y:ys) = False
usuariosDePublicacionSonUsuariosDeRed (x:xs) [] = True
usuariosDePublicacionSonUsuariosDeRed (x:xs) ((usuario, _, _):ys) | perteneceUsuario (usuario) (x:xs) == False = False
                                                                  | otherwise = usuariosDePublicacionSonUsuariosDeRed (x:xs) ys

usuariosDeLikeDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDeLikeDePublicacionSonUsuariosDeRed [] (y:ys) = False
usuariosDeLikeDePublicacionSonUsuariosDeRed (x:xs) [] = True
usuariosDeLikeDePublicacionSonUsuariosDeRed (x:xs) ((_,_,usuarios):ys) | usuariosLikeValidos (x:xs) (usuarios) == False = False
                                                                       | otherwise = usuariosDeLikeDePublicacionSonUsuariosDeRed (x:xs) ys

usuariosLikeValidos :: [Usuario] -> [Usuario] -> Bool
usuariosLikeValidos [] (x:xs) = False
usuariosLikeValidos usuarios [] = True
usuariosLikeValidos usuarios (x:xs) | perteneceUsuario x usuarios == True = usuariosLikeValidos usuarios xs                                  
                                    | otherwise = False

hacerListaUsuariosPublicaciones:: [Publicacion] -> [Integer]
hacerListaUsuariosPublicaciones [] = []
hacerListaUsuariosPublicaciones ((us, _, _):xs) = (idDeUsuario(us): hacerListaUsuariosPublicaciones(xs))

hacerListaPublicaciones:: [Publicacion] -> [String]
hacerListaPublicaciones [] = []
hacerListaPublicaciones ((_, publicacion, _):xs) = ((publicacion): hacerListaPublicaciones(xs))

perteneceString :: String -> [String] -> Bool
perteneceString a [] = False
perteneceString a (x:xs) | a == x = True 
                   | otherwise = perteneceString a xs

noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas [] = True
noHayPublicacionesRepetidas ((usuario,publicacion,_):xs)| pertenece (idDeUsuario(usuario)) (hacerListaUsuariosPublicaciones(xs)) == True && perteneceString (publicacion) (hacerListaPublicaciones(xs)) ==True =False
                                                        |otherwise = noHayPublicacionesRepetidas (xs)   

publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas (x:xs) [] = True
publicacionesValidas [] (y:ys) = False
publicacionesValidas (x:xs) (y:ys) | usuariosDePublicacionSonUsuariosDeRed (x:xs) (y:ys)==True && usuariosDeLikeDePublicacionSonUsuariosDeRed (x:xs) (y:ys)==True && noHayPublicacionesRepetidas (y:ys)==True = True
                                   |otherwise = False

redSocialValida :: RedSocial -> Bool
redSocialValida ([], _, _) = False
redSocialValida (usuarios, relaciones, publicaciones) | usuariosValidos usuarios == True && relacionesValidas (usuarios) (relaciones) == True && publicacionesValidas (usuarios) (publicaciones) == True = True
                                                      | otherwise = False
                                                      
proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (x:xs) = (nombreDeUsuario(x):proyectarNombres(xs))

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (u, r, p) a = quitarRepetidos (listaDeAmigos (u, r, p) a)

primerElemento :: Relacion -> Usuario
primerElemento (x, y) = x

segundoElemento :: Relacion -> Usuario
segundoElemento (x, y) = y

listaDeAmigos :: RedSocial -> Usuario -> [Usuario]
listaDeAmigos (u, [], p) a = []
listaDeAmigos (u, (x : xs), p) a | a == primerElemento x = (segundoElemento x : listaDeAmigos (u, (xs), p) a)
                                 | a == segundoElemento x = (primerElemento x : listaDeAmigos (u, (xs), p) a)
                                 | otherwise = listaDeAmigos (u, (xs), p) a

primerInteger :: (Integer,Integer) -> Integer
primerInteger (x,y) = x 

segundoInteger :: (Integer,Integer) -> Integer 
segundoInteger (x,y) = y

quitarRepetidos :: [Usuario] -> [Usuario]
quitarRepetidos [a] = [a]
quitarRepetidos (x:xs) | perteneceUsuario x xs == True = quitarRepetidos xs
                       | otherwise = (x: quitarRepetidos xs )


-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos (us,rs,ps) usuario | redSocialValida (us,rs,ps) == False && usuarioValido usuario == False && perteneceUsuario usuario us == False = error "RedSocial o Usuario No cumple con los requisitos"
                                    | otherwise = auxCantidadDeAmigos (us,rs,ps) usuario 

auxCantidadDeAmigos :: RedSocial -> Usuario -> Integer
auxCantidadDeAmigos (_,rs,_) usuario = perteneceRelacionInt (idDeUsuario (usuario)) (hacerListaRelacion rs) 

perteneceRelacionInt :: Integer -> [(Integer,Integer)] -> Integer
perteneceRelacionInt a [] = 0 
perteneceRelacionInt a (x:xs) | a == primerInteger (x) || a == segundoInteger (x) = 1 + perteneceRelacionInt a xs 
                                | otherwise = perteneceRelacionInt a xs 

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos (us,rs,ps) = maximoDeAmigos (hacerListaAmigos (us,rs,ps) us)

cantidadDeAmigosUsuario :: RedSocial -> Usuario -> (Usuario, Integer)
cantidadDeAmigosUsuario red usuario = (usuario,cantidadDeAmigos red usuario)

hacerListaAmigos ::  RedSocial -> [Usuario] ->  [(Usuario, Integer)]
hacerListaAmigos red [] = []
hacerListaAmigos red (x:xs) = cantidadDeAmigosUsuario red x : hacerListaAmigos red xs 

primerElementoUsuario :: (Usuario, Integer) ->  Usuario
primerElementoUsuario (us, int) = us

segundoElementoUsuario :: (Usuario, Integer) -> Integer
segundoElementoUsuario (us, int) = int

maximoDeAmigos :: [(Usuario, Integer)] -> Usuario
maximoDeAmigos [x] = primerElementoUsuario  x
maximoDeAmigos (x:y:xs) | segundoElementoUsuario  x > segundoElementoUsuario  y = maximoDeAmigos (x:xs)
                        | otherwise = maximoDeAmigos (y:xs)

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],[],_) = False
estaRobertoCarlos (us,rs,ps) | cantidadDeAmigos (us,rs,ps) (maximoDeAmigos (hacerListaAmigos (us,rs,ps) us)) > 10 = True
                                | otherwise = False


-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (usuarios, relaciones, publicaciones) (id,nombre) | tripleValidacion (usuarios, relaciones, publicaciones) (id,nombre) == True = todasLasPublicacionesDe publicaciones (id,nombre)
                                                                    |otherwise =  error "El usuario, relacion o publicacion estan mal definidas"

todasLasPublicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
todasLasPublicacionesDe [] (_,_) = []
todasLasPublicacionesDe (x:xs) (id,nombre) | id == head (hacerListaUsuariosPublicaciones (x:xs)) = x : todasLasPublicacionesDe (xs) (id,nombre)
                                            | otherwise = todasLasPublicacionesDe (xs) (id,nombre)

tripleValidacion :: RedSocial -> Usuario -> Bool
tripleValidacion (usuarios, relaciones, publicaciones) (id,nombre) | (redSocialValida (usuarios, relaciones, publicaciones) == True) && (usuarioValido (id,nombre) == True) && (perteneceUsuario (id,nombre) (usuarios) == True) = True
                                                        | otherwise = False

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red usuario| redSocialValida red == True && usuarioValido usuario == True && perteneceUsuario usuario (usuarios red) == True = listapublicacionesDeLike (publicaciones (red)) usuario

listapublicacionesDeLike:: [Publicacion] -> Usuario -> [Publicacion]
listapublicacionesDeLike [] usuario = []
listapublicacionesDeLike (x:xs) usuario| perteneceUsuario usuario (likesDePublicacion x) == True = (x: listapublicacionesDeLike xs usuario)
                                       |otherwise =  listapublicacionesDeLike xs usuario

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2| redSocialValida red == True && usuarioValido u1 == True && usuarioValido u2 == True && perteneceUsuario u1 (usuarios red) == True && perteneceUsuario u2 (usuarios red) == True && publicacionesQueLeGustanA red u1 ==publicacionesQueLeGustanA red u2 = True
                                         | otherwise = False

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel (us,rs,ps) usuario | longitud (todasLasPublicacionesDe ps usuario) > 0 && usuarioMasRepetido (hacerListaDeLikes (todasLasPublicacionesDe (ps) usuario) usuario) == longitud (todasLasPublicacionesDe (ps) usuario) = True
                                       | otherwise = False
 

hacerListaDeLikes :: [Publicacion] -> Usuario -> [Usuario]
hacerListaDeLikes [] u = []
hacerListaDeLikes ((us, pub, like):xs) u| u == us = likesDePublicacion (us,pub,like)++hacerListaDeLikes xs u
                                        | otherwise = hacerListaDeLikes xs u

usuarioMasRepetido :: [Usuario] -> Integer
usuarioMasRepetido [] = error "Lista Vacia"
usuarioMasRepetido [x] = 0
usuarioMasRepetido (x:y:xs) | (nroDeRepeticiones (x:xs) x) >= (nroDeRepeticiones (x:xs) y) =  1 + usuarioMasRepetido (x:xs)
                            | otherwise = usuarioMasRepetido (y:xs)


nroDeRepeticiones :: [Usuario] -> Usuario -> Integer
nroDeRepeticiones [] u = 0
nroDeRepeticiones (x:xs) u | x == u = 1 + nroDeRepeticiones xs u
                            | otherwise = nroDeRepeticiones xs u 

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario ->Bool
existeSecuenciaDeAmigos red u1 u2| tripleValidacion red u1 && tripleValidacion red u2 && cadenaDeAmigos (auxSeqDeAmigos (usuarios red) u1 u2) red == True && sonDeLaRed red (auxSeqDeAmigos (usuarios red) u1 u2) == True && (longitud (auxSeqDeAmigos (usuarios red) u1 u2) >=2) = True
                                 | otherwise = False

relacionadoDirecto :: Usuario -> Usuario ->RedSocial ->Bool
relacionadoDirecto u1 u2 (_, [], _) = False  
relacionadoDirecto u1 u2 (_, rs, _) |perteneceRelacion (u1,u2) rs  == True ||perteneceRelacion (u2,u1) rs == True = True
                                    |otherwise = False 

cadenaDeAmigos :: [Usuario] -> RedSocial ->Bool
cadenaDeAmigos [] red= False
cadenaDeAmigos [x] red = False
cadenaDeAmigos [x, y] red| relacionadoDirecto x y red == True = True
cadenaDeAmigos (x:y:xs) red | relacionadoDirecto x y red == True = cadenaDeAmigos (y:xs) red
                             | otherwise = False

empiezaConDeUsuarios :: [Usuario] -> Usuario
empiezaConDeUsuarios [] = error "No hay usuarios en la lista"
empiezaConDeUsuarios [x] = x
empiezaConDeUsuarios listadeusuarios=head(listadeusuarios)

terminaConDeUsuarios :: [Usuario] -> Usuario
terminaConDeUsuarios [] = error "No hay usuarios en la lista"
terminaConDeUsuarios [x] = x
terminaConDeUsuarios (_:xs)=terminaConDeUsuarios xs

sonDeLaRed :: RedSocial -> [Usuario] ->Bool
sonDeLaRed red [] = True
sonDeLaRed red (x:xs)| perteneceUsuario x (usuarios red) == True = sonDeLaRed red xs
                            | otherwise = False

sacaUltimoUsuario :: [Usuario] -> [Usuario] 
sacaUltimoUsuario [] = []
sacaUltimoUsuario [x] = []
sacaUltimoUsuario (x:xs) = x : sacaUltimoUsuario xs

sacaPrimero :: [Usuario] -> [Usuario]
sacaPrimero [] = []
sacaPrimero (x:xs) = xs


auxSeqDeAmigos :: [Usuario] -> Usuario -> Usuario -> [Usuario]
auxSeqDeAmigos [] u1 u2 = []
auxSeqDeAmigos (x:y:xs) u1 u2| x /= u1 && terminaConDeUsuarios (y:xs) /= u2 =auxSeqDeAmigos (sacaPrimero (sacaUltimoUsuario (x:y:xs))) u1 u2
                             | x == u1 && terminaConDeUsuarios(y:xs) /= u2 = [x] ++ auxSeqDeAmigos (sacaUltimoUsuario (y:xs)) y u2
                             | x /= u1 && terminaConDeUsuarios(y:xs) == u2 = auxSeqDeAmigos (sacaPrimero (x:y:xs)) u1 u2
                             | x == u1 && terminaConDeUsuarios (y:xs) == u2 = (x:y:xs)
