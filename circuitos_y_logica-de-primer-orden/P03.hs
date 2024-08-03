{-
Práctica 3. Circuitos y lógica de primer orden
Flores Morán Julieta Melina
Zarco Romero José Antonio
-}

module P03 where
import Data.Set
-- Circuitos lógicos --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Tipo de dato
data Bit = Cero | Uno deriving Eq

instance Show Bit where
  show Cero = "0"
  show Uno = "1"
  
{- 1
Funciones que simulen las compuertas lógicas NOT, AND, OR, NAND, NOR, XOR y XNOR
-}
c_not :: Bit -> Bit
c_not Cero  = Uno
c_not Uno  = Cero

c_and :: Bit -> Bit-> Bit
c_and Uno Uno = Uno
c_and _ _ = Cero


c_or :: Bit -> Bit-> Bit
c_or Cero Cero = Cero
c_or _ _ = Uno

c_nand ::Bit -> Bit-> Bit
c_nand Uno Uno = Cero
c_nand _ _ = Uno

c_nor :: Bit -> Bit-> Bit
c_nor Cero Cero = Uno
c_nor _ _ = Cero

c_xor :: Bit -> Bit-> Bit
c_xor x y = if x == y then Cero else Uno

c_xnor :: Bit -> Bit-> Bit
c_xnor x y = if x ==y then Uno else Cero


{- 2
Define una función que simule un half-adder. Esto es que sea de la forma:
full_add x y = (x+y, xy)
-}
half_add :: Bit ->Bit -> [Bit]
half_add x y = [c_xor x y , c_and x y ]

{- 3
Define una función que simule el full-adder
-}
full_add ::Bit -> Bit -> Bit -> [Bit]
full_add x y z = [c_xor (c_xor x y ) z, c_or (c_and x y) (c_and z (c_xor x y))]


  {- 4
Define funciones que simulen el flip-flop D, flip-flop JK y flip-flop T. Considera un argumento de estado. Por ejemplo:
flip-flop-d q d = d
Donde q es el estado del circuito y d el bit de entrada
-}
flip_flop_d :: Bit -> Bit -> Bit
flip_flop_d _ d = d

flip_flop_jk :: Bit -> Bit -> Bit -> Bit
flip_flop_jk q j k = c_or  (c_and j (c_not q)) (c_and (c_not k) q )

flip_flop_t :: Bit -> Bit-> Bit
flip_flop_t q t = c_xor q t

-- Lógica de primer orden ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Estructura de dato. Define estructuras de lógica de primer orden
data Pred = PTrue
           | PFalse
           | Predicado Nombre [Term] 
           | Neg Pred
           | Conj Pred Pred
           | Disy Pred Pred
           | Impl Pred Pred
           | Syss Pred Pred
           | PTodo Nombre Pred
           | Existe Nombre Pred
          deriving Eq
          
data Term = Var Nombre | Fun Nombre [Term] deriving Eq

type Nombre = String

-- Simbolos
instance Show Pred where 
    show PFalse = "False"
    show PTrue = "True"
    show (Predicado x y ) = x ++ "( "++ show y ++" )"
    show (Neg p) = "¬" ++ show p
    show (Conj p q) = "( " ++ show p ++ " ∧ " ++ show q ++ " )"
    show (Disy p q) = "( " ++ show p ++ " ∨ " ++ show q ++ " )"
    show (Impl p q) = "( " ++ show p ++ " ⟹  " ++ show q ++ " )"
    show (Syss p q) = "( " ++ show p ++ " ⟺  " ++ show q ++ " )"
    show (PTodo x y) = "∀" ++ x ++ " ( " ++ show y ++ " )"
    show (Existe x y) = "∃" ++ x ++ " ( " ++ show y ++ " )"

instance Show Term where
  show (Var n) =  n
  show (Fun n [t]) = "Fun" ++ n  ++  show t 
  


{- 1:
Crear una función que regrese las variables de una fórmula predicativa
-- auxuliar que saque las variables de aquí y que quite repeticiones de variables
-}

vars :: Pred -> [Nombre]
vars x = quitarDuplicados $ vars'(x)
  where
    vars' :: Pred -> [Nombre]
    vars' x = case x of
      PTrue     -> []
      PFalse    -> []
      (Predicado _ y)   -> auxVars y
      (Neg p) -> (vars' p)
      (Conj p q)    -> (vars' p)  ++ (vars' q)
      (Disy p q)    -> (vars' p) ++ (vars' q)
      (Impl p q)    -> (vars' p) ++ (vars' q)
      (Syss p q)    -> (vars' p) ++ (vars' q)
      (PTodo _ y) -> vars' y
      (Existe _ y) -> vars' y
      
quitarDuplicados :: (Ord a) => [a] -> [a]
quitarDuplicados = toList . fromList

auxVars :: [Term] -> [Nombre]
auxVars [] = []
auxVars (x:xs) = sacarTerm x ++ auxVars xs
  where
    sacarTerm :: Term -> [Nombre]
    sacarTerm (Var n) = [n]
    sacarTerm (Fun _ t)  = auxVars [head t]  ++  auxVars (tail t)
{- 2
Definir una función que tome una fórmula predicativa y devuelva las variables libres
@param una fórmula predicativa
@return las variables libres
-}

quitarVar :: Nombre ->  [Nombre] -> [Nombre]
quitarVar _ [] = []
quitarVar n l  = [x | x <- l, x/=n]
--quitarVar v (x:xs) = if x == v then [] ++ quitarVar v xs else [x] ++ quitarVar v xs

libres :: Pred -> [Nombre]
libres x = quitarDuplicados $ varsLibres'(x)
  where
    varsLibres' :: Pred -> [Nombre]
    varsLibres' x = case x of
      PTrue     -> []
      PFalse    -> []
      (Predicado _ y)   ->  auxVars y 
      (Neg p) -> (varsLibres' p)
      (Conj p q)    -> (varsLibres' p)  ++ (varsLibres' q)
      (Disy p q)    -> (varsLibres' p) ++ (varsLibres' q)
      (Impl p q)    -> (varsLibres' p) ++ (varsLibres' q)
      (Syss p q)    -> (varsLibres' p) ++ (varsLibres' q)
      (PTodo x y) ->  quitarVar x (varsLibres' y) 
      (Existe x y) -> quitarVar x (varsLibres' y) 
    
{- 3
Definir una función que tome una fórmula predicativa y devuelva las variables ligadas
@param una fórmula predicativa
@return las variables ligadas
-}

ligadas :: Pred -> [Nombre]
ligadas x = quitarDuplicados $ varsLigadas'(x)
  where
    varsLigadas' :: Pred -> [Nombre]
    varsLigadas' (PTodo x y) =  [x] ++ (varsLigadas' y)
    varsLigadas' (Existe x y) =    [x] ++ (varsLigadas' y) 
    varsLigadas' PTrue     = []
    varsLigadas' PFalse    = []
    varsLigadas' (Predicado _ y)   =  []
    varsLigadas' (Neg p)  = (varsLigadas' p)
    varsLigadas' (Conj p q)    = (varsLigadas' p)  ++ (varsLigadas' q)
    varsLigadas' (Disy p q)    = (varsLigadas' p) ++ (varsLigadas' q)
    varsLigadas'(Impl p q)    = (varsLigadas' p) ++ (varsLigadas' q)
    varsLigadas' (Syss p q)    = (varsLigadas' p) ++ (varsLigadas' q)
     -- Opcion para no considerar las varaibles dummy:
    -- varsLigadas' (PTodo x y) = if (contiene x (vars y)) then [x] ++ (varsLigadas' y) else (varsLigadas' y)
    -- varsLigadas' (Existe x y) =   if (contiene x (vars y)) then [x] ++ (varsLigadas' y) else (varsLigadas' y)

-- contiene :: Nombre -> [Nombre] ->Bool
-- contiene x [] = False
-- contiene x (a:xs) = if x == a then True else (contiene x  xs)

{- 4
Definir una función que tome un predicado en forma negativa y regrese su equivalente variando el cuantificador 
@param un predicado en forma negativa
@return equivalente variando el cuantificador 
-}
equiv :: Pred -> Pred
equiv  a@PTrue     = a
equiv  a@PFalse    = a
equiv a@(Predicado _ y)   =  a
equiv (Conj p q)    = Conj (equiv p)  (equiv q)
equiv (Disy p q)    = Disy (equiv p)  (equiv q)
equiv (Impl p q)    = Impl (equiv p)  (equiv q)
equiv (Syss p q)    = Syss (equiv p)  (equiv q)
equiv (Neg (PTodo x y)) =  equiv (Existe x (Neg (equiv y)))
equiv (Neg (Existe x y)) =  equiv (PTodo x (Neg (equiv y)))
equiv (Neg p)  = Neg (equiv p)
equiv (PTodo x y )=  PTodo x (equiv y)
equiv (Existe x y)  =  Existe x (equiv y)

{- 5
Define una función que cuente el número de cuantificadores en un predicado
-}
cuantificadores :: Pred -> Int
cuantificadores  PTrue     = 0
cuantificadores  PFalse    = 0
cuantificadores (Predicado _ _)   =  0
cuantificadores (Neg p)  =   (cuantificadores p)
cuantificadores (Conj p q)    = (cuantificadores p) + (cuantificadores q)
cuantificadores (Disy p q)    =   (cuantificadores p) +  (cuantificadores q)
cuantificadores (Impl p q)    =  (cuantificadores p) +  (cuantificadores q)
cuantificadores (Syss p q)    =  (cuantificadores p) + (cuantificadores q)
cuantificadores (PTodo _ t) = 1 + cuantificadores t
cuantificadores (Existe _ t) = 1 + cuantificadores t

{- 6
Define una función que cuente el número de conectivos lógicos en un predicado
-}

conectivos :: Pred -> Int
conectivos  PTrue     = 0
conectivos  PFalse    = 0
conectivos (Predicado _ _)   =  0
conectivos (Neg p)  =  1 +  (conectivos p)
conectivos (Conj p q)    = 1 +  (conectivos p) + (conectivos q)
conectivos (Disy p q)    = 1 +  (conectivos p) +  (conectivos q)
conectivos (Impl p q)    = 1 +  (conectivos p) +  (conectivos q)
conectivos (Syss p q)    = 1 +  (conectivos p) + (conectivos q)
conectivos (PTodo _ t) = conectivos t
conectivos (Existe _ t) = conectivos t
