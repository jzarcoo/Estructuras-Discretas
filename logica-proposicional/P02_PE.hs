-- Práctica 2. Logica proposicional - Punto extra
-- Flores Morán Julieta Melina
-- Zarco Romero José Antonio

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

{- 1
@param una fórmula con doble implicación 
@return su equivalente con conectores básicos según la equivalencia:  A↔B ≡ (¬A∨B) ∧ (¬B∨A)
-}
equiv_sii :: LProp -> LProp
equiv_sii = dobleNeg . equiv_sii'

-- Función auxiliar
equiv_sii' :: LProp -> LProp
equiv_sii' a@PTrue = a
equiv_sii' a@PFalse = a
equiv_sii' a@(Var p) = a
equiv_sii' (Neg p) =  Neg (equiv_sii' p)
equiv_sii' (Disy p q) =  Disy (equiv_sii' p) (equiv_sii' q)
equiv_sii' (Conj p q) =  Conj (equiv_sii' p) (equiv_sii' q)
equiv_sii' (Impl p q) =  Impl (equiv_sii' p) (equiv_sii' q)
equiv_sii' (Syss p q) =  Conj (equiv_sii' (Disy (Neg p) q)) (equiv_sii' (Disy (Neg q) p))


{- 2
@param una fórmula con doble implicación 
@return su equivalente con conectores básicos según la equivalencia según la equivalencia: A↔B ≡ (A∧B) ∨ (¬A∧¬B)
-}
equiv_syss :: LProp -> LProp
equiv_syss = dobleNeg . equiv_syss'

-- Función auxiliar
equiv_syss' :: LProp -> LProp
equiv_syss' a@PTrue = a
equiv_syss' a@PFalse = a
equiv_syss' a@(Var p) = a
equiv_syss' (Neg p) =  Neg (equiv_syss' p)
equiv_syss' (Disy p q) =  Disy (equiv_syss' p) (equiv_syss' q)
equiv_syss' (Conj p q) =  Conj (equiv_syss' p) (equiv_syss' q)
equiv_syss' (Impl p q) =  Impl (equiv_syss' p) (equiv_syss' q)
equiv_syss' (Syss p q) =  Disy (equiv_syss' (Conj p q)) (equiv_syss' (Conj (Neg p) (Neg q)))
   
{- 3
@param un número entero n
@return la suma de los primeros n+1 cuadrados
-}
nCuadrados :: Integer -> Integer
nCuadrados n
  | n < 0 = error "El número debe ser positivo"
  | otherwise = sum [x*x | x <- [0..n]]
  -- Opción con recusión:
  -- | n == 0 = 0
  -- | otherwise = n*n + nCuadrados (n-1)
  

{-
Función auxiliar
@param una formula con doble negación
@return su equivalente eliminando la doble negación
-}
dobleNeg :: LProp  -> LProp
dobleNeg a@PTrue = a
dobleNeg a@PFalse = a
dobleNeg a@(Var p) = a
dobleNeg (Neg(Neg q)) = dobleNeg q
dobleNeg (Neg p) = Neg (dobleNeg p)
dobleNeg (Disy p q) = Disy (dobleNeg p) (dobleNeg q)
dobleNeg (Conj p q) = Conj (dobleNeg p) (dobleNeg q)
dobleNeg (Impl p q) = Impl (dobleNeg p) (dobleNeg q)
dobleNeg (Syss p q) = Syss (dobleNeg p) (dobleNeg q)
