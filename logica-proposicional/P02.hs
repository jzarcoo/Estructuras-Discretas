-- Práctica 2. Logica proposicional
-- Flores Morán Julieta Melina
-- Zarco Romero José Antonio

module P02 where
import Data.Set

--Estructura de dato
data LProp = PTrue
           | PFalse
           | Var Nombre
           | Neg LProp
           | Conj LProp LProp
           | Disy LProp LProp
           | Impl LProp LProp
           | Syss LProp LProp
           deriving Eq

-- Asignaciones 
type Nombre = String
type Asignacion = [(String, Bool)]

-- Simbolos
instance Show LProp where 
    show PFalse = "False"
    show PTrue = "True"
    show (Var p) = p
    show (Neg p) = "¬" ++ show p
    show (Conj p q) = "( " ++ show p ++ " ∧ " ++ show q ++ " )"
    show (Disy p q) = "( " ++ show p ++ " ∨ " ++ show q ++ " )"
    show (Impl p q) = "( " ++ show p ++ " ⟹  " ++ show q ++ " )"
    show (Syss p q) = "( " ++ show p ++ " ⟺  " ++ show q ++ " )"

--Funciones
{- 1
@param una fórmula
@return las variables en la fórmula
-}
vars :: LProp -> [String]
vars x = quitarDuplicados $ vars'(x)
  where
    vars' :: LProp -> [String]
    vars' x = case x of
      PTrue     -> []
      PFalse    -> []
      (Var p)   -> [p]
      (Neg p) -> (vars' p)
      (Conj p q)    -> (vars' p)  ++ (vars' q)
      (Disy p q)    -> (vars' p) ++ (vars' q)
      (Impl p q)    -> (vars' p) ++ (vars' q)
      (Syss p q)    -> (vars' p) ++ (vars' q)
      
    quitarDuplicados :: (Ord a) => [a] -> [a]
    quitarDuplicados = toList . fromList
    
{- 2
@param una formula proposicional 
@return  valor aplicando las leyes de De Morgan
-}
deMorgan :: LProp -> LProp
deMorgan x = dobleNeg $ deMorgan' x
  where
    deMorgan' :: LProp -> LProp
    deMorgan' a@PTrue = a
    deMorgan' a@PFalse = a
    deMorgan' a@(Var p) = a
    deMorgan' (Neg PTrue) = PFalse
    deMorgan' (Neg PFalse) = PTrue
    deMorgan' (Neg (Conj p q)) =deMorgan' (Disy (Neg (deMorgan' p))  (Neg (deMorgan' q)))
    deMorgan' (Neg (Disy p q)) = deMorgan' (Conj  (Neg (deMorgan' p)) (Neg (deMorgan' q)))
    deMorgan' (Neg p) =  Neg (deMorgan' p)
    deMorgan' (Disy p q) = Disy (deMorgan' p) (deMorgan' q)
    deMorgan' (Conj p q) = Conj (deMorgan' p) (deMorgan' q)
    deMorgan' (Impl p q) = Impl (deMorgan' p) (deMorgan' q)
    deMorgan' (Syss p q) = Syss (deMorgan' p) (deMorgan' q)
    
{- 3
@param una formula con implicación 
@return su equivalente con conectores basicos
-}
equiv_op :: LProp -> LProp
equiv_op x = dobleNeg $ equiv_op' x
  where
    equiv_op' :: LProp -> LProp
    equiv_op' a@PTrue = a
    equiv_op' a@PFalse = a
    equiv_op' a@(Var p) = a
    equiv_op' (Neg p) =  Neg (equiv_op' p)
    equiv_op' (Disy p q) =  Disy (equiv_op' p) (equiv_op' q)
    equiv_op' (Conj p q) =  Conj (equiv_op' p) (equiv_op' q)
    equiv_op' (Impl p q) = Disy (Neg (equiv_op' p)) (equiv_op' q)
    equiv_op' (Syss p q) =  Conj (equiv_op' (Impl p q)) (equiv_op' (Impl q p))

{- 4
@param una formula con doble negación
@return Elimina la doble negación
-}
dobleNeg :: LProp  -> LProp
dobleNeg a@PTrue = a
dobleNeg a@PFalse = a
dobleNeg a@(Var p) = a
dobleNeg (Neg(Neg (q))) = dobleNeg q
dobleNeg (Neg p) = Neg (dobleNeg p)
dobleNeg (Disy p q) = Disy (dobleNeg p) (dobleNeg q)
dobleNeg (Conj p q) = Conj (dobleNeg p) (dobleNeg q)
dobleNeg (Impl p q) = Impl (dobleNeg p) (dobleNeg q)
dobleNeg (Syss p q) = Syss (dobleNeg p) (dobleNeg q)

{- 5
Funcion recursiva para contar el número de conectivos lógicos de una fórmula lógica
@param Una fórmula lógica
@return Número de conectivos de la fórmula lógica
-}
num_conectivos :: LProp -> Int
num_conectivos PTrue = 0
num_conectivos PFalse = 0
num_conectivos (Var q) = 0
num_conectivos (Neg p) = 1 + num_conectivos p
num_conectivos (Conj p q) = 1 + num_conectivos p + num_conectivos q
num_conectivos (Disy p q) = 1 + num_conectivos p + num_conectivos q
num_conectivos (Impl p q) = 1 + num_conectivos p + num_conectivos q
num_conectivos (Syss p q) = 1 + num_conectivos p + num_conectivos q

{- 6
Funcion recursiva para contar el número de variables de una fórmula lógica
@param Una fórmula lógica
@return Número de variables de la fórmula lógica
-}
num_variables :: LProp -> Int
num_variables x = length l
  where l = vars x

-- Opción con duplicados
-- num_variables PTrue = 0
-- num_variables PFalse = 0
-- num_variables (Var q) = 1
-- num_variables (Neg p) = num_variables p
-- num_variables (Conj p q) = num_variables p + num_variables q
-- num_variables (Disy p q) = num_variables p + num_variables q
-- num_variables (Impl p q) = num_variables p + num_variables q
-- num_variables (Syss p q) = num_variables p + num_variables q


{- 7
Funcion recursiva que regresa la profundidad de una fórmula lógica
@param Una fórmula lógica
@return Profundidad de la fórmula lógica
-}
profundidad :: LProp  -> Int
profundidad PTrue = 0
profundidad PFalse = 0
profundidad (Var p) = 0
profundidad (Neg p) = 1 + profundidad p
profundidad (Conj p q) = 1 + max (profundidad p) (profundidad q)
profundidad (Disy p q) = 1 + max (profundidad p) (profundidad q)
profundidad (Impl p q) = 1 + max (profundidad p) (profundidad q)
profundidad (Syss p q) = 1 + max (profundidad p) (profundidad q)

{- 8
@param una fórmula y una asignación de las variables
@return los valores de verdad 
-} 
interpretación :: LProp -> Asignacion -> Bool
interpretación PTrue _ = True  
interpretación PFalse _ = False 
interpretación (Neg p) as 
  | interpretación p as =  interpretación PFalse as
  | otherwise = interpretación  PTrue as
interpretación (Conj p q) as 
  | interpretación p as && interpretación q as = interpretación  PTrue as
  | otherwise = interpretación PFalse as
interpretación (Disy p q) as 
  | not (interpretación p as) && not (interpretación q as) = interpretación PFalse as
  | otherwise = interpretación  PTrue as
interpretación (Impl p q) as 
  | (interpretación p as) && not (interpretación q as) = interpretación PFalse as
  | otherwise = interpretación  PTrue as
interpretación (Syss p q) as 
  | (interpretación p as)==(interpretación q as) = interpretación  PTrue as
  | otherwise = interpretación PFalse as
interpretación a@(Var p) as = (buscaValor a as)
  where
    buscaValor :: LProp -> Asignacion -> Bool
    buscaValor p [] = error "Asignación de una variable no encontrada" 
    buscaValor p ((var, valor):xs)
      | p == (Var var) = valor
      | otherwise = buscaValor p xs  

  
