import Test.HUnit
import Solucion


run = runTestTT testEJ

testEJ = test [
    "publicacionesDe 1" ~: publicacionesDe redA usuario2 ~?= [publicacion2_1, publicacion2_2],
    "publicaciionesDe 2" ~: publicacionesDe redA usuario1 ~?= [publicacion1_1, publicacion1_2],
    "cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,
    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,
    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True
 ]
--    "usuarioConMasAmigos 1" ~:  usuarioConMasAmigos redA ~?= [usuario2, usuario4]
usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

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

-- todasLasPublicaciones [((1, "Juan"), "Este es mi primer post", [(2, "Natalia"), (4, "Mariela")]),((1, "Juan"), "Este es mi tercer post", [(2, "Natalia"), (5, "Natalia")]),((2, "Natalia"), "Good Bye World",[(1, "Juan"),(4, "Mariela")])] (2, "Natalia")
