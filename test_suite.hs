import Test.HUnit
import Solucion

main :: IO ()
main = putStrLn "Hola, mundo!"

run = runTestTT tests

tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],
    " nombresDeUsuarios 2" ~: (nombresDeUsuarios redB) ~?= ["Juan","Natalia","Pedro","Natalia"],
    " nombresDeUsuarios 3" ~: (nombresDeUsuarios redC) ~?= error "Red Social no cumple los requisitos", -- Catch Error 
    " nombresDeUsuarios 4" ~: (nombresDeUsuarios redD) ~?= error "Red Social no cumple los requisitos", -- Catch Error 

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],
    " amigosDe 2" ~: (amigosDe redB usuario3) ~?= [usuario2],
    " amigosDe 3" ~: (amigosDe redB usuario5) ~?= [],       -- Este Test hace lo esperado pero tira error por consola, puede que falte un caso base bara un usuario que no tiene relaciones dentro de la red. 
    " amigosDe 4" ~: (amigosDe redB usuario1) ~?= [usuario2],

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6,"Hector")
usuario7 = (7,"Luis")
usuario8 = (8,"") -- Usuario Mal creado
usuario9 = (8, "Carlos") -- usaurio con id repetido

usuariosC = [ usuario1, usuario5, usuario6, usuario7,usuario8 ] -- Existe un Usuario ma cleado en esta lista
usuariosD = [usuario3,usuario4,usuario5,usuario8,usuario9]
redC = (usuariosC, relacionesB, publicacionesA) -- RedSocial con un Usuario en UsuariosC no valido
redD = (usuariosD, relacionesA,publicacionesA) -- RedSocial con un Id de usuario repetido

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

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