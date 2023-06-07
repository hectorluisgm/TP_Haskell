module Tests where
import Test.HUnit
import Solucion

main = runTestTT todosLosTests 

todosLosTests = test [testDeNombresDeUsuarios, testDeAmigosDe, testDeCantidadDeAmigos,testDeUsuarioConMasAmigos, testDeEstaRobertoCarlos, testDePublicacionesDe, testDePubliacaionesQueLeGustaA, testDeLesGustanLasMismasPublicaciones, testDeTieneUnSeguidorFiel, testDeExisteSecuenciaDeAmigos]

testDeNombresDeUsuarios = test [
    " nombresDeUsuarios 1 test de la catedra" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],
    " nombresDeUsuarios 2 Red con nombres de usuario repetidos pero IDs distintos" ~: (nombresDeUsuarios redB) ~?= ["Juan","Pedro","Natalia"],
    " nombresDeUsuarios 3 RedC con muchos mas usuarios" ~: (nombresDeUsuarios redC) ~?= ["Juan","Pedro","Mariela","Natalia","Jose","Hector","Ignacio","Milagros","Nicolas"],
    " nombresDeUsuarios 4 RedD con la misma cantidad de usuarios pero sin publicaciones" ~: (nombresDeUsuarios redD) ~?= ["Juan","Pedro","Mariela","Natalia","Jose","Hector","Ignacio","Milagros","Nicolas"]]

testDeAmigosDe = test[
    " amigosDe 1 test de la catedra" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],
    " amigosDe 2 usuario3 con una sola relacion" ~: (amigosDe redB usuario3) ~?= [usuario2],
    " amigosDe 3 usuario5 con cero relaciones en redB" ~: (amigosDe redB usuario5) ~?= [],    
    " amigosDe 4 usuario3 con mas de dos relaciones en redC" ~: (amigosDe redC usuario3) ~?= [usuario1, usuario2, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]]

testDeCantidadDeAmigos = test [
    " cantidadDeAmigos 1 test de la catedra" ~: (cantidadDeAmigos redA usuario1) ~?= 2,
    " cantidadDeAmigos 2 Usuario con un unico amigo" ~: (cantidadDeAmigos redB usuario3) ~?= 1,
    " cantidadDeAmigos 3  Usuario sin amigos" ~: (cantidadDeAmigos redB usuario5) ~?= 0,
    " cantidadDeAmigos 4 Usuario con mas de diez amigos" ~: (cantidadDeAmigos redD usuario4) ~?= 11]

testDeUsuarioConMasAmigos = test [
    " usuarioConMasAmigos 1 test de la catedra" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],
    " usuarioConMasAmigos 2 Usuario relacionado con todos los usuarios de la Red" ~: expectAny (usuarioConMasAmigos redC) [usuario3],
    " usuarioConMasAmigos 3 Usuarios con la misma cantidad de amigos en la redD puede ser uno u otro" ~: expectAny (usuarioConMasAmigos redD) [usuario3,usuario4]]

testDeEstaRobertoCarlos = test [
    " estaRobertoCarlos 1 test de la catedra" ~: (estaRobertoCarlos redA) ~?= False,
    " estaRobertoCarlos 2 con un usuario que lo cumple" ~: (estaRobertoCarlos redC) ~?= True,
    " estaRobertoCarlos 3 con mas de un usuario que lo cumple" ~: (estaRobertoCarlos redD) ~?= True,
    " estaRobertoCarlos 4 en una red sin relaciones" ~: (estaRobertoCarlos redI) ~?= False]

testDePublicacionesDe = test [
    " publicacionesDe 1 test de la catedra" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],
    " publicacionesDe 2 en una red sin publicaciones" ~: (publicacionesDe redC usuario6) ~?= [],
    " publicacionesDe 3 con muchas publicaciones" ~: (publicacionesDe redE usuario1) ~?= [publicacion1_2, publicacion1_1, publicacion1_3, publicacion1_4, publicacion1_5],
    " publicacionesDe 4 usuario sin publicaciones" ~: (publicacionesDe redE usuario2) ~?= []]

testDePubliacaionesQueLeGustaA = test [
    " publicacionesQueLeGustanA 1 test de la catedra" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],
    " publicacionesQueLeGustanA 2 Usuario a que se da like a sus propias publicaciones" ~: (publicacionesQueLeGustanA redA usuario3) ~?= [((3,"Pedro"),"Lorem Ipsum",[(3,"Pedro")]),((3,"Pedro"),"dolor sit amet",[(2,"Natalia"),(3,"Pedro")])],
    " publicacionesQueLeGustanA 3 Usuario con al que le gusta una unica publicacion en la Red" ~: (publicacionesQueLeGustanA redF usuario4) ~?= [publicacion1_1]]

testDeLesGustanLasMismasPublicaciones = test [
    " lesGustanLasMismasPublicaciones 1 Usuarios que NO le gustan las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= False,
    " lesGustanLasMismasPublicaciones 2 Usuarios a los que les gustan las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones redG usuario2 usuario5) ~?= True,
    " lesGustanLasMismasPublicaciones 3 Usuarios a los que NO les gustan las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones redC usuario2 usuario4) ~?= False]

testDeTieneUnSeguidorFiel = test [
    " tieneUnSeguidorFiel 1 test de la catedra" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,
    " tieneUnSeguidorFiel 2 UsuarioConPublicacionesSinSeguidorFiel" ~: (tieneUnSeguidorFiel redH usuario1) ~?= False,
    " tieneUnSeguidorFiel 3 SinPublicaciones" ~: (tieneUnSeguidorFiel redB usuario5) ~?= False,
    " tieneUnSeguidorFiel 4 sinLikesEnUnaPublicacion" ~: (tieneUnSeguidorFiel redX usuario4) ~?= False,
    " tieneUnSeguidorFiel 5 ySoloUnaPublicacion" ~: (tieneUnSeguidorFiel redZ usuario3) ~?= True,
    " tieneUnSeguidorFiel 6 sinLikesEnTodasSusPublicaciones" ~: (tieneUnSeguidorFiel redW usuario3) ~?= False,
    " tieneUnSeguidorFiel 7 Usuario que se dio like a todas sus publicaciones" ~: (tieneUnSeguidorFiel redC usuario3) ~?= False]


testDeExisteSecuenciaDeAmigos = test [
    " existeSecuenciaDeAmigos 1 test de la catedra" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True,
    " existeSecuenciaDeAmigos 2 SoloSonAmigosLosUsusariosDelInput" ~: (existeSecuenciaDeAmigos redY usuario1 usuario2) ~?= True,
    " existeSecuenciaDeAmigos 3 SinRelacionDirectaEntreUsusariosDelInput" ~: (existeSecuenciaDeAmigos redB usuario1 usuario3) ~?= True,
    " existeSecuenciaDeAmigos 4 NoHayCadenaDeAmigos" ~: (existeSecuenciaDeAmigos redV usuario3 usuario12) ~?= False,
    " existeSecuenciaDeAmigos 5 SinRelaciones" ~: (existeSecuenciaDeAmigos redI usuario3 usuario10) ~?= False,
    " existeSecuenciaDeAmigos 6 MismaCantidadDeAmigosEntreSi" ~: (existeSecuenciaDeAmigos redT usuario3 usuario4) ~?= True,
    " existeSecuenciaDeAmigos 7 ConUsuariosInvertidos" ~: (existeSecuenciaDeAmigos redB usuario3 usuario1) ~?= True,
    " existeSecuenciaDeAmigos 8 ConUsuariosDeEntradaIguales" ~: (existeSecuenciaDeAmigos redB usuario1 usuario1) ~?= True,
    " existeSecuenciaDeAmigos 10 Usuario con Ninguna Relacion, se relaciona con el mismo" ~: (existeSecuenciaDeAmigos redB usuario5 usuario5) ~?= False
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

relacion5_12 = (usuario5, usuario12)

relacion7_5 = (usuario7, usuario5)

relacion12_8 =(usuario12, usuario8)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])
publicacion1_6 = (usuario1, "Hola, soy Us1", [usuario3,usuario7])
publicacion1_7 = (usuario1, "Hola, cómo va'", [usuario5])
publicacion1_8 = (usuario1, "Hola", [usuario7])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [usuario3])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2,usuario3])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario3, usuario5])
publicacion3_4 = (usuario3, "Lorem Ipsum", [])
publicacion3_5 = (usuario3, "Lorem Ipsu", [])
publicacion3_6 = (usuario3, "Lorem Ips", [])


publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])
publicacion4_4 = (usuario4, "hahaha", [usuario5, usuario6]) 

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

usuariosX= [usuario4, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesX = [relacion4_6, relacion4_7, relacion4_8, relacion4_9, relacion4_10, relacion4_11, relacion4_12]
publicacionesX = [publicacion4_2, publicacion4_4]
redX = (usuariosX, relacionesX, publicacionesX)


usuariosZ = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesZ = [relacion1_3, relacion2_3, relacion3_4, relacion3_5, relacion3_6, relacion3_7, relacion3_8, relacion3_9, relacion3_10, relacion3_11, relacion3_12, relacion1_4, relacion2_4, relacion4_5,  relacion4_7, relacion4_8, relacion4_9, relacion4_10, relacion4_11, relacion4_12]
publicacionesZ = [publicacion1_7, publicacion3_3, publicacion1_5, publicacion1_6 ]
redZ = (usuariosZ, relacionesZ, publicacionesZ)


usuariosW = [usuario1, usuario3, usuario5, usuario7] 
relacionesW= [relacion1_3, relacion3_5, relacion3_7, relacion7_5 ]
publicacionesW= [publicacion3_4, publicacion3_5, publicacion3_6]
redW = (usuariosW, relacionesW, publicacionesW)

usuariosY = [usuario1, usuario2, usuario3, usuario5]
relacionesY= [relacion1_2]
publicacionesY = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redY = (usuariosY, relacionesY, publicacionesY)



usuariosV = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6, usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesV = [relacion1_3, relacion2_3, relacion3_4, relacion5_12, relacion12_8]
publicacionesV = [publicacion1_3, publicacion3_3]
redV = (usuariosV, relacionesV, publicacionesV)


usuariosT = [usuario1, usuario2, usuario3, usuario4, usuario5, usuario6,usuario7, usuario8, usuario9, usuario10, usuario11, usuario12]
relacionesT = [relacion3_4, relacion4_5, relacion4_6, relacion4_7, relacion3_5, relacion3_6, relacion3_7]
publicacionesT = [publicacion1_1, publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redT= (usuariosT, relacionesT, publicacionesT)