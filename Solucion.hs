module Solucion where


-- Completar con los datos del grupo
-- Nombre de Grupo: Debuggers
-- Integrante 1: Ignacio Martin Vittorini Fennema, nachovittfenn@gmail.com, LU 460/20
-- Integrante 2: Milagros Guadalupe Villagran, milagrosgvillagran@gmail.com, LU 58/21
-- Integrante 3: Nicolas Cestau, nicocestau@gmail.com, LU 834/23
-- Integrante 4: Hector Gomez Moya, hectorluisgomezmoya@gmail.com, LU 921/23

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

-- Ejercicio 1:
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios (red))
-- Qué hace la función: En principio, la funcion nombresDeUsuarios requiere por especificacion que la red social sea valida, cosa que se hace en la primer linea de codigo. En caso de que no se cumpla la condicion, da un error, y al cumplirse, usa la funcion auxiliar proyectar nombres para recorrer la lista de usuarios de la red social, extraer el string de nombre de la tupla del tipo Usuario y generar una nueva lista con cada uno de esos strings.

--Auxiliares--
perteneceString :: String -> [String] -> Bool
perteneceString a [] = False
perteneceString a (x:xs) | a == x = True 
                        | otherwise = perteneceString a xs

quitarRepetidos :: [String] -> [String]
quitarRepetidos [] = []
quitarRepetidos [a] = [a]
quitarRepetidos (x:xs) | perteneceString x xs = quitarRepetidos xs
                       | otherwise = x : quitarRepetidos xs

proyectarNombres :: [Usuario] -> [String]
proyectarNombres [] = []
proyectarNombres (x:xs) = quitarRepetidos ( (nombreDeUsuario x) : proyectarNombres xs)


perteneceUsuario :: Usuario -> [Usuario] -> Bool
perteneceUsuario a [] = False
perteneceUsuario a (x:xs) | a == x = True 
                          | otherwise = perteneceUsuario a xs


-- Ejercicio 2
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us,[],ps) a = []
amigosDe (us, (x:xs), ps) a | a == primerElemento x = (segundoElemento x : amigosDe (us, (xs), ps) a)
                            | a == segundoElemento x = (primerElemento x : amigosDe (us, (xs), ps) a)
                            | otherwise = amigosDe (us, (xs), ps) a
-- Qué hace la función: Esta función utiliza una lista de amigos del usuario mediante sus relaciones en una red social dada para generar una lista de todos sus amigos. En caso de haber amistades repetidas en distintas relaciones, solamente se nombran una vez gracias a la función quitar repetidos.

--Auxiliares--
primerElemento :: Relacion -> Usuario
primerElemento (x, y) = x

segundoElemento :: Relacion -> Usuario
segundoElemento (x, y) = y


-- Ejercicio 3 
cantidadDeAmigos :: RedSocial -> Usuario -> Integer
cantidadDeAmigos red usuario  = longitud (amigosDe red usuario)
-- Qué hace la función: La funcion cantidadDeAmigos comienza validando los requerimientos de la especificacion y en caso de que no se cumplan devuelve un error. Si se validan, devuelve la longitud de la lista de tipo Usuario obtenida de aplicar la funcion AmigosDe al usuario entrada y usando, tambien, la red social de entrada.

--Auxiliares--
longitud :: [t] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

-- Ejercicio 4
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ([u],_,_) = u
usuarioConMasAmigos ((x:y:xs),rs,ps) | (cantidadDeAmigos ((x:y:xs),rs,ps) x) > (cantidadDeAmigos ((x:y:xs),rs,ps) y) = usuarioConMasAmigos ((x:xs),rs,ps)
                                        | otherwise = usuarioConMasAmigos ((y:xs),rs,ps)

--Qué hace la función: La función usuarioConMasAmigos mediante una lista que contiene a cada usuario con su respectiva cantidad de amigos, calcula cuál es el número máximo de amigos de aquella lista, una vez obtenido, devuelve el nombre del usuario con su id


--Ejercicio 5
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],_,_) = False
estaRobertoCarlos ((x:xs),rs,ps) | cantidadDeAmigos ((x:xs),rs,ps) x > 10 = True
                                    | otherwise = estaRobertoCarlos (xs,rs,ps)

--Qué hace la función: La función estaRobertoCarlos la cual recibe una RedSocial y devuelve un Bool , esta función se apoya sobre 3 funciones auxiliares y una comparación en base si la cantidad de amigos es mayor a 10 , quiere decir que existe Roberto Carlos y nos devuelve un True , de otra forma devuelve un False. Para lograr esto nos apoyamos en la función cantidadDeAmigos ,pero filtramos el usuario al que pasamos como parámetro (el cual pertenece a la RedSocial), con la otra función auxiliar MáximoDeAmigos, la cual nos devuelve el Usuario con más relaciones en toda la RedSocial, por lo tanto si las relaciones de este usuario son mayores a diez se dice que estaRobertoCarlos ósea True.

-- Ejercicio 6
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe (_,_,[]) usuario = []
publicacionesDe (us, rs, (x:xs)) usuario | usuario == usuarioDePublicacion (x) = x : publicacionesDe (us,rs, xs) usuario
                                        | otherwise = publicacionesDe (us,rs, xs) usuario

--Qué hace la función: La función publicacionesDe recibe un RedSocial y un usuario , el usuario debe pertenecer a la red social para ello usamos un función auxiliar tripleValidacion , donde validamos si el usuario pertenece a la red , si el usuario es válido y por último si la RedSocial pasada es válida, por otro lado esta función usa otra función auxiliar todasLasPublicacionesDe la cual será la que de nuestro retorno, esta función crea un nueva lista con Solo las publicaciones que fueron creadas por el usuario pasado como parámetro, devolviendo así una lista de todas sus publicaciones de la RedSocial.
                                                                  
-- Ejercicio 7 
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (us,rs,[]) usuario = []
publicacionesQueLeGustanA (us,rs,(x:xs)) usuario | perteneceUsuario usuario (likesDePublicacion x) = (x : publicacionesQueLeGustanA (us,rs,(xs)) usuario)
                                                    | otherwise = publicacionesQueLeGustanA (us,rs,(xs)) usuario

--Qué hace la función: La funcion publicacionQueLeGustanA comienza verificando los requerimientos de la especificacion. En caso de cumplirse devuelve una lista creada por listapublicacionesDeLike, que se genera chequeando que el usuario de entrada pertenezca a los likes de la primera publicacion de la red social, y en caso de cumplirse esa condicion, añade esa primera publicacion a la lista devuelta y chequea las siguientes publicaciones de forma recursiva, hasta recorrer toda la lista.

-- Ejercicio 8
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2| publicacionesQueLeGustanA red u1 == publicacionesQueLeGustanA red u2 = True
                                         | otherwise = False
--Qué hace la función: Se verifica que se cumplan todas las condiciones requeridas por la especificacion, y se le agrega otra, en donde, utilizando al funcion definida por el ejercicio 7, publicacionesQueLeGustanA, se igual dos listas generadas cada una por uno de los 2 usuarios de entrada y al ser iguales, se devuelve un True

-- Ejercicio 9 
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel (us, rs, ps) usuario  | longitud(publicacionesDe (us, rs, ps) usuario) /= 0 = auxSeguidorFiel (publicacionesDe (us, rs, ps) usuario) us usuario
                                            | otherwise = False
--Qué hace la función: Esta función recibe una redSocial y un Usuario y evalua si el mismo tiene al menos un usuario de la red que le haya dado like a todas sus publicaciones.
--Para ello arma una lista de todos los likes que recibieron las publicaciones realizadas por el usuario del input, busca dentro de esta lista al usuario que mas veces se repita, esto quiere decir, que es el usuario que mas likes le dio a las publicaciones, y si el numero de apariciones de este usuario en la lista de likes es igual a la cantidad de posteos del usuario del input, entonces el mismo tiene un seguidor fiel.

--Auxiliares--
auxSeguidorFiel :: [Publicacion] -> [Usuario] -> Usuario -> Bool 
auxSeguidorFiel [] us usuario = True
auxSeguidorFiel ps [] usuario = False
auxSeguidorFiel (y:ys) (x:xs) usuario | (usuario /= x) && perteneceUsuario x (likesDePublicacion y ) = auxSeguidorFiel ys (x:xs) usuario
                                        | otherwise = auxSeguidorFiel (y:ys) xs usuario

-- Ejercicio 10

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2  | longitud(amigosDe red u1) ==  0 || longitud(amigosDe red u2) == 0 = False
                                    | u1 == u2 && longitud(amigosDe red u1) >= 1 = True
                                    | perteneceUsuario u2 (amigosDe red u1) = True
                                    | perteneceUsuario u2 (yaVistos red (amigosDe red u1) u2) = True
                                    | otherwise = False
--Qué hace la función: La funcion verifica los requerimientos de la especificacion, y nos da como resultado un bool. Se genera una nueva lista recortando la de usuarios de la red hasta que empiece y termine con los 2 usuarios que de entrada. Luego, sobre esa lista se aplica cadenadeAmigos y sonDeLaRed para verificar si efectivamente se cumple que el usuario inicial y el siguiente estan relacionados sucesivamente hasta el ultimo, y si los usuarios de la lista pertenecen a la red. Por ultimo, tambien se verifica la longitud de la nueva lista.



--Auxiliares--
-- La lista de usuarios que recibe yaVistos sera la lista de amigosDe U1 llamada en la funcion principal
-- El usuario sera el U2 pasado por parametro en la funcion principal
yaVistos :: RedSocial -> [Usuario] -> Usuario -> [Usuario]
yaVistos red [] u = []
yaVistos red (x:xs) u | perteneceUsuario u (amigosDe red x) = amigosDe red x
                        | otherwise = amigosDe red x ++ yaVistos red xs u
