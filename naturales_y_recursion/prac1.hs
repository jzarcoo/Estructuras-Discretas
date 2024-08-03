--Práctica 1. Números naturales y recursión
-- Flores Morán Julieta Melina
-- Zarco Romero José Antonio

--Práctica: Naturales

--Estructura de dato

data Natural = Cero | S Natural deriving Show 

--Funciones

{- 1
Convierte un número entero a su estructura de dato Natural
@param número entero (0,1,2,...)
@return estructura de dato Natural
-}
a_natural :: Integer -> Natural
a_natural 0 = Cero
a_natural n
    | n > 0 = S (a_natural(n-1))
    | otherwise = error "Número negativo no pertenece a los naturales"

{- 2
Convierte una estructura de dato Natural a su valor entero
@param estructura de dato Natural
@return número entero (0,1,2,...)
-}
a_entero :: Natural -> Integer
a_entero Cero = 0
a_entero (S n) = a_entero(n) + 1

{- 3
Suma sobre la estructura de dato Natural
@param dos estructuras de dato Natural
@return suma de dos estructucturas de dato Natural
-}
suma_nat :: Natural -> Natural -> Natural
suma_nat n Cero = n
suma_nat n (S(m)) = S (suma_nat (n) (m))
--suma_nat n m = a_natural(a_entero(n) + a_entero(m))

{- 4
Multiplicación sobre estructura de dato Natural
@param dos estructuras de dato Natural
@return multiplicación de dos estructucturas de dato Natural
-}
mult_nat :: Natural -> Natural -> Natural
mult_nat n Cero = Cero
mult_nat n (S(m)) = suma_nat (mult_nat (n)(m)) (n)


--Práctica: Recursión

--Funciones

{- 1
Obtiene el número Fibonacci en base a la posición
@param posición n
@return número Fibonacci en posición n
-}
fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n 
     | n > 0 = fibo(n-1) + fibo(n-2)
     | n < 0 = error "No se puede calcular el valor de posiciones negativas"

{- 2
Define multiplicación entre 2 enteros
@param entero1 entero2
@return resultado de multiplicación
-}
mult :: Integer -> Integer -> Integer
mult n m
     | (n==0 || m==0) = 0
     | otherwise = n + (mult n (m-1))


{- 3
Define potencia
@param base
@param exponente
@return potencia
-}
potencia :: Integer -> Integer -> Integer
potencia _ 0 = 1
potencia n m 
     | m > 0 = n * (potencia n (m-1))
     | otherwise = error "No se pueden calcular potencias negativas. No pertenece a los naturales"


{- 4
Define factorial
@param número entero n
@return factorial de n
-}
fact :: Integer -> Integer
fact 0 = 1
fact n
     | n > 0 = n * fact(n-1)
     | n < 0 = error "No se puede calcular el factorial de un número negativo"

{- 5
Define algoritmo de división en base a la función
@param una tupla del cociente y el residuo
@return una tupla del cociente y el residuo
-}
f :: (Integer, Integer) -> (Integer, Integer)
f (x, r) = (x+1, r)

divR :: Integer -> Integer -> (Integer, Integer)
divR a b 
     | (a < 0) || (b < 0) = error "No se pueden usar en números negativos" 
     | b == 0 = error "No se pude dividir entre 0"
     | a < b = (0, a)
     | a >= b = f(divR (a-b) (b))




--Práctica: Árboles

--Estructura recursiva

data ArbolB a = Vacio | Nodo a (ArbolB a) (ArbolB a) deriving (Eq, Show)
--t1 = Nodo "t1" Vacio Vacio --e.g.

--Funciones

{- 1
Calcula el número de nodos del árbol
@param árbol binario 
@return número de nodos del árbol
-}
nn :: ArbolB a -> Integer
nn  Vacio = 0
nn (Nodo _ t1 t2) = 1 + nn(t1)+ nn (t2)

{- 2
Calcula la profundidad de un árbol
@param árbol binario 
@return profundidad de un árbol
-}
dep :: ArbolB a -> Integer
dep Vacio = 0
dep (Nodo _ t1 t2) = 1+ max (dep (t1)) (dep (t2)) 
