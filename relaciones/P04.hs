module P04 where

{-
Práctica 4. Relaciones
Flores Morán Julieta Melina
Zarco Romero José Antonio
-}

--Tipo relación
type Relacion = [(Int, Int)] 
type Tupla = (Int, Int)

-- Funciones auxiliares
{-
Verifica si un elemento está o no en una lista dada.
@param un elemento
@param una lista
@return True si el elemento está en la lista, False en otro caso
-}
contiene :: (Eq a) => a -> [a] -> Bool
contiene _ [] = False
contiene e (x:xs) = (e == x) || contiene e xs

-- Funciones
{- 1
Genera una función eRepetidos que elimine los elementos repetidos de una lista.
Por ejemplo: eRepetidos [0,1,0,1,2] → [0,1,2]
@param una lista
@return la lista sin repetidos
-}
eRepetidos :: (Eq a) => [a] -> [a]
eRepetidos [] = []
-- Opción con filter
eRepetidos (x:xs) = x : eRepetidos (filter (/=x) xs)
-- Opción sin filter
--eRepetidos (x:xs) = (if contiene x xs then [] else [x]) ++  eRepetidos xs
    
{- 2
Escribe una función ́listaElem elimine los elementos repetidos de una relación (lista de tuplas).
Por ejemplo: ́listaElem [(1, 1), (2, 3), (5, 3)] = [1, 2, 3, 5]
@param una relacion
@return una lista de enteros sin repetidos
-}
listaElem :: Relacion -> [Int]
listaElem [] = []
-- Opción sin filter
listaElem ((a, b) : xs) = eRepetidos (a : b : listaElem xs)
-- Opción con filter
-- listaElem ((a, b) : xs) = eRepetidos (a : b : listaElem (filter (\(x, y) -> x /= a || y /= b) xs))
 
{- 3
Escribe una función reflexiva que regrese un valor booleano True si la relacion es reflexiva, o False si no lo es.
@param una relación
@return True si la relación es reflexiva, False si no lo es.
-}
reflexiva :: Relacion -> Bool
reflexiva [] = True
reflexiva r = and (map (\(a, b) -> pertenece (a, a) r && pertenece (b, b) r) r)

{- Solución sin map
reflexiva r = reflexiva' r r
   where
     reflexiva' :: Relacion -> Relacion -> Bool
     reflexiva' [] _ = True
     reflexiva' r@((a,b) :xs) ro = if (pertenece  (a, a) ro) && (pertenece  (b, b) ro) then (reflexiva' xs ro) else False
-}

{- 4
Da una función pertenece que regrese un valor booleano si una tupla pertenece a una relacion. Por ejemplo  ́pertenece (0,1) [(0,1),(0,2),(2,1)] → True.
@param una tupla
@param una relación
@return True si el elemento pertenece a la relación, False en otro caso.
-}
pertenece :: Tupla -> Relacion -> Bool
pertenece _ [] = False
pertenece t@(c, d) ((a, b) : xs) = (a == c && b == d) || pertenece t xs

{- 5
Define una función asimetrica que regrese True cuando la relación es asimétrica, y False en otro caso.
@param una relación
@return True si la relación es asimetrica, False si no lo es
-}
asimetrica  :: Relacion -> Bool
asimetrica [] = True
asimetrica r@((a, b) : xs) = (not (pertenece (b, a) r) && asimetrica xs)

{- 6
Da una funcion ́transitiva que regrese True cuando la relacion es transitiva, y False en otro caso.
@param una relación
@return True si la relación es transitiva, False si no lo es
-}
transitiva :: Relacion -> Bool
transitiva [] = True
transitiva r = and (map (\e -> pertenece e r) listaTransitiva)
  where
    listaTransitiva = composicion r r

{- Solución sin map
... r = contencion r lista
-- Podemos ahorrarnos el where reutilizando la función de composición
where 
lista = eRepetidos [(a, c) | (a, b1) <- r, (b2, c) <- r, b1 == b2]

contencion :: Relacion -> Relacion -> Bool
contencion _ [] = True
contencion r1 ((a, b) : xs) = if pertenece (a, b) r1 then contencion r1 xs else False 
-}

{- 7
Define una función unionLab que regrese la unión de dos relaciones.
@param dos relaciones
@return la unión de dos relaciones
-}
unionLab :: Relacion -> Relacion -> Relacion
-- Opción con filter
unionLab r1 r2 = r1 ++ filter (\e -> not (pertenece e r1)) r2
-- Opción con listas de comprensión:
-- unionLab r1 r2 = r1 ++ [e | e <- r2, not (pertenece e r1)]

{- 8
Define una función ́interseccion que regrese la intersección de dos relaciones.
@param dos relaciones
@return la intersección de dos relaciones
-}
interseccion :: Relacion -> Relacion -> Relacion
-- Opción con filter
--interseccion r1 r2 = filter (\e -> pertenece e r2) r1
-- Opción con listas de comprensión:
-- interseccion r1 r2 = [e | e <- r1, pertenece e r2]
-- Opción con recursión
interseccion [] _ = []
interseccion (x:xs) r2 = (if pertenece x r2 then [x] else []) ++ (interseccion xs r2)

{- 9
Da una función composicion que tome dos relaciones R,S y regrese la relación compuesta R◦S.
@param dos relaciones
@return la relación compuesta
-}
composicion :: Relacion -> Relacion -> Relacion
-- Opción con listas de comprensión:
composicion r1 r2 = [(a, c) | (a, b1) <- r1, (b2, c) <- r2, b1 == b2]

{- 10
Define una función ́inversa que regrese la relación inversa de la relación de entrada.
@param una relación
@return la relación inversa
-}
inversa :: Relacion -> Relacion
-- Opción con listas de comprensión:
inversa r = [(b, a) | (a, b) <- r]
-- Opción con recursión
-- inversa [] = []
-- inversa ((a, b) : xs) = (b, a) : inversa xs
