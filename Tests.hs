import Test.HUnit
import Solucion

main :: IO ()
main = putStrLn "Hola, mundo!"

run = runTestTT tests

tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],
    " nombresDeUsuarios 2" ~: (nombresDeUsuarios redB) ~?= ["Juan","Natalia","Pedro","Natalia"],
    " nombresDeUsuarios 3" ~: (nombresDeUsuarios redC) ~?= ["Juan","Natalia","Pedro","Mariela", "Natalia", "Jose", "Hector", "Ignacio", "Milagros", "Nicolas", "Nicolas", "Nicolas"],
    " nombresDeUsuarios 4" ~: (nombresDeUsuarios redD) ~?= ["Juan","Natalia","Pedro","Mariela", "Natalia", "Jose", "Hector", "Ignacio", "Milagros", "Nicolas", "Nicolas", "Nicolas"],

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],
    " amigosDe 2" ~: (amigosDe redB usuario3) ~?= [usuario2],
    " amigosDe 3" ~: (amigosDe redB usuario5) ~?= [],    
    " amigosDe 4" ~: (amigosDe redB usuario1) ~?= [usuario2],

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,
    " cantidadDeAmigos 2" ~: (cantidadDeAmigos redB usuario3) ~?= 1,
    " cantidadDeAmigos 3" ~: (cantidadDeAmigos redB usuario5) ~?= 0,
    " cantidadDeAmigos 4" ~: (cantidadDeAmigos redD usuario4) ~?= 11,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],
    " usuarioConMasAmigos 2" ~: expectAny (usuarioConMasAmigos redC) [usuario3],
    " usuarioConMasAmigos 3" ~: expectAny (usuarioConMasAmigos redD) [usuario4],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,
    " estaRobertoCarlos 2" ~: (estaRobertoCarlos redC) ~?= True,
    " estaRobertoCarlos 3" ~: (estaRobertoCarlos redD) ~?= True,
    " estaRobertoCarlos 4 en una red sin relaciones" ~: (estaRobertoCarlos redI) ~?= False,

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],
    " publicacionesDe 2" ~: (publicacionesDe redC usuario6) ~?= [],
    " publicacionesDe 3" ~: (publicacionesDe redE usuario1) ~?= [publicacion1_2, publicacion1_1, publicacion1_3, publicacion1_4, publicacion1_5],
    " publicacionesDe 4 usuario sin publicaciones" ~: (publicacionesDe redE usuario2) ~?= [],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],
    " publicacionesQueLeGustanA 2" ~: (publicacionesQueLeGustanA redA usuario3) ~?= [],
    " publicacionesQueLeGustanA 3" ~: (publicacionesQueLeGustanA redF usuario4) ~?= [publicacion1_1],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,
    " lesGustanLasMismasPublicaciones 1" ~: (lesGustanLasMismasPublicaciones redG usuario2 usuario5) ~?= True,
    " lesGustanLasMismasPublicaciones 3" ~: (lesGustanLasMismasPublicaciones redC usuario2 usuario4) ~?= False,

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,
    " tieneUnSeguidorFiel 2" ~: (tieneUnSeguidorFiel redH usuario1) ~?= False,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Jose")
usuario7 = (7, "Hector")
usuario8 = (8, "Ignacio")
usuario9 = (9, "Milagros")
usuario10 = (10, "Nicolas")
usuario11 = (11, "Nicolas")
usuario12 = (12, "Nicolas")


relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

--relaciones extra--
relacion3_5 = (usuario5, usuario3)
relacion3_6 = (usuario6, usuario3)
relacion3_7 = (usuario7, usuario3)
relacion3_8 = (usuario8, usuario3)
relacion3_9 = (usuario9, usuario3)
relacion3_10 = (usuario10, usuario3)
relacion3_11 = (usuario11, usuario3)
relacion3_12 = (usuario12, usuario3)

relacion4_5 = (usuario5, usuario4)
relacion4_6 = (usuario6, usuario4)
relacion4_7 = (usuario7, usuario4)
relacion4_8 = (usuario8, usuario4)
relacion4_9 = (usuario9, usuario4)
relacion4_10 = (usuario10, usuario4)
relacion4_11 = (usuario11, usuario4)
relacion4_12 = (usuario12, usuario4)

relacion7_5 = (usuario7, usuario5)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])
publicacion1_6 = (usuario1, "Hola, soy Us1", [usuario3,usuario7])
publicacion1_7 = (usuario1, "Hola, cómo va'", [usuario5])
publicacion1_8 = (usuario1, "Hola", [usuario7])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)


--Redes nuevas

usuariosC = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesC = [relacion1_3, relacion2_3, relacion3_4, relacion3_5, relacion3_6, relacion3_7, relacion3_8, relacion3_9, relacion3_10, relacion3_11, relacion3_12]
publicacionesC = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redC = (usuariosC, relacionesC, publicacionesC)

usuariosD = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesD = [relacion1_3, relacion2_3, relacion3_4, relacion3_5, relacion3_6, relacion3_7, relacion3_8, relacion3_9, relacion3_10, relacion3_11, relacion3_12, relacion1_4, relacion2_4, relacion4_5, relacion4_6, relacion4_7, relacion4_8, relacion4_9, relacion4_10, relacion4_11, relacion4_12]
publicacionesD = []
redD = (usuariosD, relacionesD, publicacionesD)

usuariosE = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesE = [relacion1_3, relacion2_3, relacion3_4, relacion3_5, relacion3_6, relacion3_7, relacion3_8, relacion3_9, relacion3_10, relacion3_11, relacion3_12]
publicacionesE = [publicacion1_2, publicacion1_1, publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redE = (usuariosE, relacionesE, publicacionesE)

usuariosF = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6,usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesF = [relacion1_3, relacion2_3, relacion3_4, relacion3_5, relacion3_6, relacion3_7, relacion3_8, relacion3_9, relacion3_10, relacion3_11, relacion3_12]
publicacionesF = [publicacion1_1, publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redF = (usuariosF, relacionesF, publicacionesF)

usuariosG = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesG = [relacion1_3, relacion2_3, relacion3_4, relacion3_5, relacion3_6, relacion3_7, relacion3_8, relacion3_9, relacion3_10, relacion3_11, relacion3_12, relacion1_4, relacion2_4, relacion4_5, relacion4_6, relacion4_7, relacion4_8, relacion4_9, relacion4_10, relacion4_11, relacion4_12]
publicacionesG = [publicacion1_3, publicacion3_3]
redG = (usuariosG, relacionesG, publicacionesG)

usuariosH = [usuario1, usuario3, usuario5, usuario7]
relacionesH= [relacion1_3, relacion3_5, relacion3_7, relacion7_5 ]
publicacionesH= [publicacion1_5, publicacion1_6, publicacion1_7, publicacion1_8]
redH = (usuariosH, relacionesH, publicacionesH)

usuariosI = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesI = []
publicacionesI = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redI = (usuariosI, relacionesI, publicacionesI)