{-# LANGUAGE GADTs #-} -- Error al cargar por cómo están definidos los tipos algebraicos. Solución: https://stackoverflow.com/questions/32828483/how-do-you-allow-gadts-in-haskell
import P04 -- Módulo con las funciones a testear.
import Data.List -- Funciones como 'sort'...

-- Tipo de dato para pruebas con una sola respuesta.
data PruebaSimple t where PruebaS :: (Show t, Eq t) => t -> t -> PruebaSimple t
instance Show (PruebaSimple a) where 
    show (PruebaS x y) = if x == y
                         then "Pasó."
                         else "No pasó...\n  Respuesta esperada: " ++ show y ++ "\n  Respuesta dada: " ++ show x

-- Tipo de dato para pruebas con múltiples respuestas posibles.
data PruebaMultiple t where PruebaM :: (Show t, Eq t) => t -> [t] -> PruebaMultiple t
instance Show (PruebaMultiple a) where
    show (PruebaM x l)
        | elem x l = "Pasó."
        | otherwise = "No pasó...\n  Posibles respuestas: " ++ show l ++ "\n  Respuesta dada: " ++ show x

-- Relaciones.
r00 = [(1,1), (1,2), (1,4), (2,1), (2,2), (3,3), (4,1), (4,4)]
r01 = [(1,1), (1,3), (2,2), (3,2), (3,3), (4,3), (4,4)]
r02 = [(3,3)]
r03 = [(7,7), (6,6)]
r04 = [(2,1), (3,1), (3,2), (4,1), (4,2), (4,3)]
r05 = [(1,1), (1,2), (2,1), (2,2), (3,4), (4,1), (4,4)]
r06 = [(1,1), (1,2), (2,1), (2,2), (4,3), (4,1), (4,4)]
r07 = [(1,3), (1,4), (2,4), (3,2), (4,3)]
r08 = [(4,3), (3,4)]
r09 = [(4,3), (3,4), (4,4)]
r10 = [(1,1), (1,2), (1,4), (2,1), (2,2), (2,4), (3,3), (4,1), (4,2), (4,4)]
r11 = [(4,3), (3,3), (3,4), (4,4)]

-- Función principal.
main = do

    print "----- eRepetidos -----"
    print (PruebaS (sort (eRepetidos [0,2,0,1,2])) [0, 1, 2])
    print (PruebaS (sort (eRepetidos ([1..75] ++ [50..150]))) [1..150])
    print (PruebaS (sort (eRepetidos [4])) [4])
    print (PruebaS (sort (eRepetidos [1..10])) [1..10])
    print (PruebaS (sort (eRepetidos [7, 7, 7])) [7])
    print (PruebaS (sort (eRepetidos [8, 8])) [8])

    print "----- listaElem -----"
    print (PruebaS (sort (listaElem [(1,1), (2,3), (5,3)])) [1, 2, 3, 5])
    print (PruebaS (sort (listaElem [(1,2), (2,3), (4,5), (1,2)])) [1, 2, 3, 4, 5])
    print (PruebaS (sort (listaElem [(1,1), (1,1), (1,3), (3,1)])) [1, 3])
    print (PruebaS (sort (listaElem [(1,1), (1,1), (1,3)])) [1, 3])
    print (PruebaS (sort (listaElem [(x,y) | x <- [1..3], y <- [4..7]])) [1..7])
    print (PruebaS (sort (listaElem [(x,y) | x <- [1..6], y <- [2..7]])) [1..7])
    print (PruebaS (sort (listaElem [])) [])
    print (PruebaS (sort (listaElem r00)) [1..4])
    print (PruebaS (sort (listaElem r01)) [1..4])
    print (PruebaS (sort (listaElem r02)) [3])
    print (PruebaS (sort (listaElem r03)) [6, 7])
    print (PruebaS (sort (listaElem r04)) [1..4])
    print (PruebaS (sort (listaElem r05)) [1..4])
    print (PruebaS (sort (listaElem r06)) [1..4])
    print (PruebaS (sort (listaElem r07)) [1..4])
    print (PruebaS (sort (listaElem r08)) [3, 4])
    print (PruebaS (sort (listaElem r09)) [3, 4])
    print (PruebaS (sort (listaElem r10)) [1..4])
    print (PruebaS (sort (listaElem r11)) [3, 4])

    print "----- reflexiva -----"
    print (PruebaS (reflexiva []) True)
    print (PruebaS (reflexiva r00) True)
    print (PruebaS (reflexiva r01) True)
    print (PruebaS (reflexiva r02) True)
    print (PruebaS (reflexiva r03) True)
    print (PruebaS (reflexiva r04) False)
    print (PruebaS (reflexiva r05) False)
    print (PruebaS (reflexiva r06) False)
    print (PruebaS (reflexiva r07) False)
    print (PruebaS (reflexiva r08) False)
    print (PruebaS (reflexiva r09) False)
    print (PruebaS (reflexiva r10) True)
    print (PruebaS (reflexiva r11) True)

    print "----- pertenece -----"
    print (PruebaS (pertenece (0,1) [(0,1),(0,2),(2,1)]) True)
    print (PruebaS (pertenece (0,1) [(1,0),(0,2),(2,1),(1,0)]) False)
    print (PruebaS (pertenece (0,1) [(1,0),(0,2),(2,1),(0,1),(0,1)]) True)
    print (PruebaS (pertenece (4,7) []) False)
    print (PruebaS (pertenece (4,7) [(4,7)]) True)
    print (PruebaS (pertenece (-4,-7) [(-4,7), (4,-7)]) False)

    print "----- asimetrica -----"
    print (PruebaS (asimetrica []) True)
    print (PruebaS (asimetrica r00) False)
    print (PruebaS (asimetrica r01) False)
    print (PruebaS (asimetrica r02) False)
    print (PruebaS (asimetrica r03) False)
    print (PruebaS (asimetrica r04) True)
    print (PruebaS (asimetrica r05) False)
    print (PruebaS (asimetrica r06) False)
    print (PruebaS (asimetrica r07) True)
    print (PruebaS (asimetrica r08) False)
    print (PruebaS (asimetrica r09) False)
    print (PruebaS (asimetrica r10) False)
    print (PruebaS (asimetrica r11) False)

    print "----- transitiva -----"
    print (PruebaS (transitiva []) True)
    print (PruebaS (transitiva r00) False)
    print (PruebaS (transitiva r01) False)
    print (PruebaS (transitiva r02) True)
    print (PruebaS (transitiva r03) True)
    print (PruebaS (transitiva r04) True)
    print (PruebaS (transitiva r05) False)
    print (PruebaS (transitiva r06) False)
    print (PruebaS (transitiva r07) False)
    print (PruebaS (transitiva r08) False)
    print (PruebaS (transitiva r09) False)
    print (PruebaS (transitiva r10) True)
    print (PruebaS (transitiva r11) True)

    print "----- unionLab -----"
    print (PruebaS (sort (unionLab r00 r01)) [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(3,2),(3,3),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r00 r00)) [(1,1),(1,2),(1,4),(2,1),(2,2),(3,3),(4,1),(4,4)])
    print (PruebaS (sort (unionLab r00 r01)) [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(3,2),(3,3),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r00 r04)) [(1,1),(1,2),(1,4),(2,1),(2,2),(3,1),(3,2),(3,3),(4,1),(4,2),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r00 r07)) [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,4),(3,2),(3,3),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r01 r03)) [(1,1),(1,3),(2,2),(3,2),(3,3),(4,3),(4,4),(6,6),(7,7)])
    print (PruebaS (sort (unionLab r01 r06)) [(1,1),(1,2),(1,3),(2,1),(2,2),(3,2),(3,3),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r01 r09)) [(1,1),(1,3),(2,2),(3,2),(3,3),(3,4),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r01 r11)) [(1,1),(1,3),(2,2),(3,2),(3,3),(3,4),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r03 r02)) [(3,3),(6,6),(7,7)])
    print (PruebaS (sort (unionLab r03 r03)) [(6,6),(7,7)])
    print (PruebaS (sort (unionLab r03 r06)) [(1,1),(1,2),(2,1),(2,2),(4,1),(4,3),(4,4),(6,6),(7,7)])
    print (PruebaS (sort (unionLab r03 r09)) [(3,4),(4,3),(4,4),(6,6),(7,7)])
    print (PruebaS (sort (unionLab r03 r10)) [(1,1),(1,2),(1,4),(2,1),(2,2),(2,4),(3,3),(4,1),(4,2),(4,4),(6,6),(7,7)])
    print (PruebaS (sort (unionLab r04 r04)) [(2,1),(3,1),(3,2),(4,1),(4,2),(4,3)])
    print (PruebaS (sort (unionLab r04 r05)) [(1,1),(1,2),(2,1),(2,2),(3,1),(3,2),(3,4),(4,1),(4,2),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r04 r06)) [(1,1),(1,2),(2,1),(2,2),(3,1),(3,2),(4,1),(4,2),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r04 r07)) [(1,3),(1,4),(2,1),(2,4),(3,1),(3,2),(4,1),(4,2),(4,3)])
    print (PruebaS (sort (unionLab r05 r07)) [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,4),(3,2),(3,4),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r05 r08)) [(1,1),(1,2),(2,1),(2,2),(3,4),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r06 r00)) [(1,1),(1,2),(1,4),(2,1),(2,2),(3,3),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r06 r03)) [(1,1),(1,2),(2,1),(2,2),(4,1),(4,3),(4,4),(6,6),(7,7)])
    print (PruebaS (sort (unionLab r06 r06)) [(1,1),(1,2),(2,1),(2,2),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r06 r09)) [(1,1),(1,2),(2,1),(2,2),(3,4),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r07 r04)) [(1,3),(1,4),(2,1),(2,4),(3,1),(3,2),(4,1),(4,2),(4,3)])
    print (PruebaS (sort (unionLab r07 r08)) [(1,3),(1,4),(2,4),(3,2),(3,4),(4,3)])
    print (PruebaS (sort (unionLab r08 r01)) [(1,1),(1,3),(2,2),(3,2),(3,3),(3,4),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r08 r02)) [(3,3),(3,4),(4,3)])
    print (PruebaS (sort (unionLab r08 r07)) [(1,3),(1,4),(2,4),(3,2),(3,4),(4,3)])
    print (PruebaS (sort (unionLab r08 r08)) [(3,4),(4,3)])
    print (PruebaS (sort (unionLab r09 r00)) [(1,1),(1,2),(1,4),(2,1),(2,2),(3,3),(3,4),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r09 r03)) [(3,4),(4,3),(4,4),(6,6),(7,7)])
    print (PruebaS (sort (unionLab r09 r07)) [(1,3),(1,4),(2,4),(3,2),(3,4),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r09 r10)) [(1,1),(1,2),(1,4),(2,1),(2,2),(2,4),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r09 r11)) [(3,3),(3,4),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r10 r01)) [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,4),(3,2),(3,3),(4,1),(4,2),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r10 r10)) [(1,1),(1,2),(1,4),(2,1),(2,2),(2,4),(3,3),(4,1),(4,2),(4,4)])
    print (PruebaS (sort (unionLab r11 r01)) [(1,1),(1,3),(2,2),(3,2),(3,3),(3,4),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r11 r04)) [(2,1),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r11 r07)) [(1,3),(1,4),(2,4),(3,2),(3,3),(3,4),(4,3),(4,4)])
    print (PruebaS (sort (unionLab r11 r11)) [(3,3),(3,4),(4,3),(4,4)])

    print "----- interseccion -----"
    print (PruebaS (sort (interseccion r00 r01)) [(1,1),(2,2),(3,3),(4,4)])
    print (PruebaS (sort (interseccion r00 r00)) [(1,1),(1,2),(1,4),(2,1),(2,2),(3,3),(4,1),(4,4)])
    print (PruebaS (sort (interseccion r00 r01)) [(1,1),(2,2),(3,3),(4,4)])
    print (PruebaS (sort (interseccion r00 r04)) [(2,1),(4,1)])
    print (PruebaS (sort (interseccion r00 r07)) [(1,4)])
    print (PruebaS (sort (interseccion r01 r03)) [])
    print (PruebaS (sort (interseccion r01 r06)) [(1,1),(2,2),(4,3),(4,4)])
    print (PruebaS (sort (interseccion r01 r09)) [(4,3),(4,4)])
    print (PruebaS (sort (interseccion r01 r11)) [(3,3),(4,3),(4,4)])
    print (PruebaS (sort (interseccion r03 r02)) [])
    print (PruebaS (sort (interseccion r03 r03)) [(6,6),(7,7)])
    print (PruebaS (sort (interseccion r03 r06)) [])
    print (PruebaS (sort (interseccion r03 r09)) [])
    print (PruebaS (sort (interseccion r03 r10)) [])
    print (PruebaS (sort (interseccion r04 r04)) [(2,1),(3,1),(3,2),(4,1),(4,2),(4,3)])
    print (PruebaS (sort (interseccion r04 r05)) [(2,1),(4,1)])
    print (PruebaS (sort (interseccion r04 r06)) [(2,1),(4,1),(4,3)])
    print (PruebaS (sort (interseccion r04 r07)) [(3,2),(4,3)])
    print (PruebaS (sort (interseccion r05 r07)) [])
    print (PruebaS (sort (interseccion r05 r08)) [(3,4)])
    print (PruebaS (sort (interseccion r06 r00)) [(1,1),(1,2),(2,1),(2,2),(4,1),(4,4)])
    print (PruebaS (sort (interseccion r06 r03)) [])
    print (PruebaS (sort (interseccion r06 r06)) [(1,1),(1,2),(2,1),(2,2),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (interseccion r06 r09)) [(4,3),(4,4)])
    print (PruebaS (sort (interseccion r07 r04)) [(3,2),(4,3)])
    print (PruebaS (sort (interseccion r07 r08)) [(4,3)])
    print (PruebaS (sort (interseccion r08 r01)) [(4,3)])
    print (PruebaS (sort (interseccion r08 r02)) [])
    print (PruebaS (sort (interseccion r08 r07)) [(4,3)])
    print (PruebaS (sort (interseccion r08 r08)) [(3,4),(4,3)])
    print (PruebaS (sort (interseccion r09 r00)) [(4,4)])
    print (PruebaS (sort (interseccion r09 r03)) [])
    print (PruebaS (sort (interseccion r09 r07)) [(4,3)])
    print (PruebaS (sort (interseccion r09 r10)) [(4,4)])
    print (PruebaS (sort (interseccion r09 r11)) [(3,4),(4,3),(4,4)])
    print (PruebaS (sort (interseccion r10 r01)) [(1,1),(2,2),(3,3),(4,4)])
    print (PruebaS (sort (interseccion r10 r10)) [(1,1),(1,2),(1,4),(2,1),(2,2),(2,4),(3,3),(4,1),(4,2),(4,4)])
    print (PruebaS (sort (interseccion r11 r01)) [(3,3),(4,3),(4,4)])
    print (PruebaS (sort (interseccion r11 r04)) [(4,3)])
    print (PruebaS (sort (interseccion r11 r07)) [(4,3)])
    print (PruebaS (sort (interseccion r11 r11)) [(3,3),(3,4),(4,3),(4,4)])

    print "----- composicion -----"
    print (PruebaS (sort (composicion r00 r01)) [(1,1),(1,2),(1,3),(1,3),(1,4),(2,1),(2,2),(2,3),(3,2),(3,3),(4,1),(4,3),(4,3),(4,4)])
    print (PruebaS (sort (composicion r00 r00)) [(1,1),(1,1),(1,1),(1,2),(1,2),(1,4),(1,4),(2,1),(2,1),(2,2),(2,2),(2,4),(3,3),(4,1),(4,1),(4,2),(4,4),(4,4)])
    print (PruebaS (sort (composicion r00 r01)) [(1,1),(1,2),(1,3),(1,3),(1,4),(2,1),(2,2),(2,3),(3,2),(3,3),(4,1),(4,3),(4,3),(4,4)])
    print (PruebaS (sort (composicion r00 r04)) [(1,1),(1,1),(1,2),(1,3),(2,1),(3,1),(3,2),(4,1),(4,2),(4,3)])
    print (PruebaS (sort (composicion r00 r07)) [(1,3),(1,3),(1,4),(1,4),(2,3),(2,4),(2,4),(3,2),(4,3),(4,3),(4,4)])
    print (PruebaS (sort (composicion r01 r03)) [])
    print (PruebaS (sort (composicion r01 r06)) [(1,1),(1,2),(2,1),(2,2),(3,1),(3,2),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (composicion r01 r09)) [(1,4),(3,4),(4,3),(4,4),(4,4)])
    print (PruebaS (sort (composicion r01 r11)) [(1,3),(1,4),(3,3),(3,4),(4,3),(4,3),(4,4),(4,4)])
    print (PruebaS (sort (composicion r03 r02)) [])
    print (PruebaS (sort (composicion r03 r03)) [(6,6),(7,7)])
    print (PruebaS (sort (composicion r03 r06)) [])
    print (PruebaS (sort (composicion r03 r09)) [])
    print (PruebaS (sort (composicion r03 r10)) [])
    print (PruebaS (sort (composicion r04 r04)) [(3,1),(4,1),(4,1),(4,2)])
    print (PruebaS (sort (composicion r04 r05)) [(2,1),(2,2),(3,1),(3,1),(3,2),(3,2),(4,1),(4,1),(4,2),(4,2),(4,4)])
    print (PruebaS (sort (composicion r04 r06)) [(2,1),(2,2),(3,1),(3,1),(3,2),(3,2),(4,1),(4,1),(4,2),(4,2)])
    print (PruebaS (sort (composicion r04 r07)) [(2,3),(2,4),(3,3),(3,4),(3,4),(4,2),(4,3),(4,4),(4,4)])
    print (PruebaS (sort (composicion r05 r07)) [(1,3),(1,4),(1,4),(2,3),(2,4),(2,4),(3,3),(4,3),(4,3),(4,4)])
    print (PruebaS (sort (composicion r05 r08)) [(3,3),(4,3)])
    print (PruebaS (sort (composicion r06 r00)) [(1,1),(1,1),(1,2),(1,2),(1,4),(2,1),(2,1),(2,2),(2,2),(2,4),(4,1),(4,1),(4,2),(4,3),(4,4),(4,4)])
    print (PruebaS (sort (composicion r06 r03)) [])
    print (PruebaS (sort (composicion r06 r06)) [(1,1),(1,1),(1,2),(1,2),(2,1),(2,1),(2,2),(2,2),(4,1),(4,1),(4,2),(4,3),(4,4)])
    print (PruebaS (sort (composicion r06 r09)) [(4,3),(4,4),(4,4)])
    print (PruebaS (sort (composicion r07 r04)) [(1,1),(1,1),(1,2),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(4,1),(4,2)])
    print (PruebaS (sort (composicion r07 r08)) [(1,3),(1,4),(2,3),(4,4)])
    print (PruebaS (sort (composicion r08 r01)) [(3,3),(3,4),(4,2),(4,3)])
    print (PruebaS (sort (composicion r08 r02)) [(4,3)])
    print (PruebaS (sort (composicion r08 r07)) [(3,3),(4,2)])
    print (PruebaS (sort (composicion r08 r08)) [(3,3),(4,4)])
    print (PruebaS (sort (composicion r09 r00)) [(3,1),(3,4),(4,1),(4,3),(4,4)])
    print (PruebaS (sort (composicion r09 r03)) [])
    print (PruebaS (sort (composicion r09 r07)) [(3,3),(4,2),(4,3)])
    print (PruebaS (sort (composicion r09 r10)) [(3,1),(3,2),(3,4),(4,1),(4,2),(4,3),(4,4)])
    print (PruebaS (sort (composicion r09 r11)) [(3,3),(3,4),(4,3),(4,3),(4,4),(4,4)])
    print (PruebaS (sort (composicion r10 r01)) [(1,1),(1,2),(1,3),(1,3),(1,4),(2,1),(2,2),(2,3),(2,3),(2,4),(3,2),(3,3),(4,1),(4,2),(4,3),(4,3),(4,4)])
    print (PruebaS (sort (composicion r10 r10)) [(1,1),(1,1),(1,1),(1,2),(1,2),(1,2),(1,4),(1,4),(1,4),(2,1),(2,1),(2,1),(2,2),(2,2),(2,2),(2,4),(2,4),(2,4),(3,3),(4,1),(4,1),(4,1),(4,2),(4,2),(4,2),(4,4),(4,4),(4,4)])
    print (PruebaS (sort (composicion r11 r01)) [(3,2),(3,3),(3,3),(3,4),(4,2),(4,3),(4,3),(4,4)])
    print (PruebaS (sort (composicion r11 r04)) [(3,1),(3,1),(3,2),(3,2),(3,3),(4,1),(4,1),(4,2),(4,2),(4,3)])
    print (PruebaS (sort (composicion r11 r07)) [(3,2),(3,3),(4,2),(4,3)])
    print (PruebaS (sort (composicion r11 r11)) [(3,3),(3,3),(3,4),(3,4),(4,3),(4,3),(4,4),(4,4)])

    print "----- inversa -----"
    print (PruebaS (sort (inversa r00)) [(1,1),(1,2),(1,4),(2,1),(2,2),(3,3),(4,1),(4,4)])
    print (PruebaS (sort (inversa r01)) [(1,1),(2,2),(2,3),(3,1),(3,3),(3,4),(4,4)])
    print (PruebaS (sort (inversa r02)) [(3,3)])
    print (PruebaS (sort (inversa r03)) [(6,6),(7,7)])
    print (PruebaS (sort (inversa r04)) [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)])
    print (PruebaS (sort (inversa r05)) [(1,1),(1,2),(1,4),(2,1),(2,2),(4,3),(4,4)])
    print (PruebaS (sort (inversa r06)) [(1,1),(1,2),(1,4),(2,1),(2,2),(3,4),(4,4)])
    print (PruebaS (sort (inversa r07)) [(2,3),(3,1),(3,4),(4,1),(4,2)])
    print (PruebaS (sort (inversa r08)) [(3,4),(4,3)])
    print (PruebaS (sort (inversa r09)) [(3,4),(4,3),(4,4)])
    print (PruebaS (sort (inversa r10)) [(1,1),(1,2),(1,4),(2,1),(2,2),(2,4),(3,3),(4,1),(4,2),(4,4)])
    print (PruebaS (sort (inversa r11)) [(3,3),(3,4),(4,3),(4,4)])
