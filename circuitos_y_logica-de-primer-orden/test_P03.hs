{-# LANGUAGE GADTs #-} -- Error al cargar por cómo están definidos los tipos algebraicos. Solución: https://stackoverflow.com/questions/32828483/how-do-you-allow-gadts-in-haskell
import P03 -- Módulo con las funciones a testear.
import Data.List -- Funciones como 'sort', 'nub'...

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

-- Variables y términos.
a = Var "a"
b = Var "b"
c = Var "c"
d = Var "d"
e = Var "e"
f = Var "f"
g = Var "g"
j = Var "j"
k = Var "k"
p = Var "p"
x = Var "x"
y = Var "y"
z = Var "z"
uno = Var "1"
cuatro = Var "4"
suma_a_b = Fun "suma" [a, b]
suma_c_b = Fun "suma" [c, b]
suma_1_1 = Fun "suma" [uno, uno]
suma_1_1_1_1 = Fun "suma" [suma_1_1, suma_1_1]
capital = Fun "capital" [p]

-- a+b = c+b => a=c.
igual_sumas = Predicado "igual" [suma_a_b, suma_c_b]
igual_ac = Predicado "igual" [a, c]
axioma = Impl igual_sumas igual_ac

-- (1+1)+(1+1) = 4
igual_sumas_4 = Predicado "igual" [suma_1_1_1_1, cuatro]

-- Ejemplo de "Matemáticas discretas", de Favio E. Miranda y Elisa Viso G.
estudiante = Predicado "estudiante" [x]
profesor = Predicado "profesor" [y]
más_joven_xy = Predicado "más_joven" [x, y]
estudiante_profe = PTodo "x" (Impl estudiante (Existe "y" (Conj profesor más_joven_xy))) -- ∀x(estudiante(x) → ∃y(profesor(y) ∧ más_joven(x, y)))

-- (∀xP(x))→(∃yP(x))
ej00 = Impl (PTodo "x" (Predicado "P" [x])) (Existe "y" (Predicado "P" [x]))
-- (∀xP(x))∧(∃yP(z))
ej01 = Conj (PTodo "x" (Predicado "P" [x])) (Existe "y" (Predicado "P" [z]))
-- (∃xP(y,y))↔∃yP(y,z))
ej02 = Syss (PTodo "x" (Predicado "P" [y, y])) (Existe "y" (Predicado "P" [y, z]))
-- ((∃xP(x, g) ∨ ∃yQ(y, g)) → ((∃zR(x) ∧ ¬∀fS(f)) ↔ ∃gT(g, z)))
ej03 = Impl (Disy (Existe "x" (Predicado "P" [x, g])) (Existe "y" (Predicado "Q" [y, g]))) (Syss (Conj (Existe "z" (Predicado "R" [x])) (Neg (PTodo "f" (Predicado "S" [f])))) (Existe "g" (Predicado "T" [g, z])))
-- ¬∀x(True ∧ ¬∃yFalse)
ej04 = Neg (PTodo "x" (Conj PTrue (Neg (Existe "y" PFalse))))
-- ¬∀x∃yTrue
ej05 = Neg (PTodo "x" (Existe "y" PTrue))

-- Función principal.
main = do

  print "----- c_not -----"
  print (PruebaS (c_not Cero) Uno)
  print (PruebaS (c_not Uno) Cero)

  print "----- c_and -----"
  print (PruebaS (c_and Cero Cero) Cero)
  print (PruebaS (c_and Cero Uno) Cero)
  print (PruebaS (c_and Uno  Cero) Cero)
  print (PruebaS (c_and Uno  Uno) Uno)

  print "----- c_or -----"
  print (PruebaS (c_or Cero Cero) Cero)
  print (PruebaS (c_or Cero Uno) Uno)
  print (PruebaS (c_or Uno  Cero) Uno)
  print (PruebaS (c_or Uno  Uno) Uno)

  print "----- c_nand -----"
  print (PruebaS (c_nand Cero Cero) Uno)
  print (PruebaS (c_nand Cero Uno) Uno)
  print (PruebaS (c_nand Uno  Cero) Uno)
  print (PruebaS (c_nand Uno  Uno) Cero)

  print "----- c_nor -----"
  print (PruebaS (c_nor Cero Cero) Uno)
  print (PruebaS (c_nor Cero Uno) Cero)
  print (PruebaS (c_nor Uno  Cero) Cero)
  print (PruebaS (c_nor Uno  Uno) Cero)

  print "----- c_xor -----"
  print (PruebaS (c_xor Cero Cero) Cero)
  print (PruebaS (c_xor Cero Uno) Uno)
  print (PruebaS (c_xor Uno  Cero) Uno)
  print (PruebaS (c_xor Uno  Uno) Cero)

  print "----- c_xnor -----"
  print (PruebaS (c_xnor Cero Cero) Uno)
  print (PruebaS (c_xnor Cero Uno) Cero)
  print (PruebaS (c_xnor Uno  Cero) Cero)
  print (PruebaS (c_xnor Uno  Uno) Uno)

  print "----- half_add -----"
  print (PruebaS (half_add Cero Cero) [Cero, Cero])
  print (PruebaS (half_add Cero Uno) [Uno, Cero])
  print (PruebaS (half_add Uno  Cero) [Uno, Cero])
  print (PruebaS (half_add Uno  Uno) [Cero, Uno])

  print "----- full_add -----"
  print (PruebaS (full_add Cero Cero Cero) [Cero, Cero])
  print (PruebaS (full_add Cero Cero Uno) [Uno, Cero])
  print (PruebaS (full_add Cero Uno  Cero) [Uno, Cero])
  print (PruebaS (full_add Cero Uno  Uno) [Cero, Uno])
  print (PruebaS (full_add Uno  Cero Cero) [Uno, Cero])
  print (PruebaS (full_add Uno  Cero Uno) [Cero, Uno])
  print (PruebaS (full_add Uno  Uno  Cero) [Cero, Uno])
  print (PruebaS (full_add Uno  Uno Uno) [Uno, Uno])

  print "----- flip_flop_d -----"
  print (PruebaS (flip_flop_d Cero Cero) Cero)
  print (PruebaS (flip_flop_d Cero Uno) Uno)
  print (PruebaS (flip_flop_d Uno  Cero) Cero)
  print (PruebaS (flip_flop_d Uno  Uno) Uno)

  print "----- flip_flop_jk -----" -- Q J K
  print (PruebaS (flip_flop_jk Cero Cero Cero) Cero)
  print (PruebaS (flip_flop_jk Cero Cero Uno) Cero)
  print (PruebaS (flip_flop_jk Cero Uno  Cero) Uno)
  print (PruebaS (flip_flop_jk Cero Uno  Uno) Uno)
  print (PruebaS (flip_flop_jk Uno  Cero Cero) Uno)
  print (PruebaS (flip_flop_jk Uno  Cero Uno) Cero)
  print (PruebaS (flip_flop_jk Uno  Uno  Cero) Uno)
  print (PruebaS (flip_flop_jk Uno  Uno Uno) Cero)

  print "----- flip_flop_t -----"
  print (PruebaS (flip_flop_t Cero Cero) Cero)
  print (PruebaS (flip_flop_t Cero Uno) Uno)
  print (PruebaS (flip_flop_t Uno  Cero) Uno)
  print (PruebaS (flip_flop_t Uno  Uno) Cero)

  print "----- vars -----"
  print (PruebaS (vars (Neg PTrue)) [])
  print (PruebaS (vars (Conj PTrue PFalse)) [])
  print (PruebaS (vars (Predicado "" [Var "hello_world"])) ["hello_world"])
  print (PruebaS (sort (vars (Predicado "" [x, Fun "g" [a, Fun "f" [b]], z]))) ["a", "b", "x", "z"])
  print (PruebaS (sort (vars igual_ac)) ["a", "c"])
  print (PruebaS (vars estudiante) ["x"])
  print (PruebaS (vars profesor) ["y"])
  print (PruebaS (sort (vars más_joven_xy)) ["x", "y"])
  print (PruebaM (sort (vars igual_sumas)) [["a", "b", "c"] , ["a", "b", "b", "c"]])
  print (PruebaM (sort (vars igual_sumas_4)) [["1", "4"] , ["1", "1", "1", "1", "4"]])
  print (PruebaM (sort (vars axioma)) [["a", "b", "c"] , ["a", "a", "b", "b", "c", "c"]])
  print (PruebaM (sort (vars estudiante_profe)) [["x", "y"] , ["x", "x", "y", "y"]])
  print (PruebaM (sort (vars ej00)) [["x"] , ["x", "x"]])
  print (PruebaS (vars ej01) ["x", "z"])
  print (PruebaM (sort (vars ej02)) [["y", "z"] , ["y", "y", "y", "z"]])
  print (PruebaM (sort (vars ej03)) [["f", "g", "x", "y", "z"] , ["f","g","g","g","x","x","y","z"]])
  print (PruebaS (vars ej04) []) -- ¬∀x(True ∧ ¬∃yFalse)
  print (PruebaS (vars ej05) []) -- ¬∀x∃yTrue

  print "----- libres -----"
  print (PruebaS (libres (Neg PTrue)) [])
  print (PruebaS (libres (Conj PTrue PFalse)) [])
  print (PruebaS (libres (Predicado "" [Var "hello_world"])) ["hello_world"])
  print (PruebaS (libres igual_ac) ["a", "c"])
  print (PruebaS (libres estudiante) ["x"])
  print (PruebaS (libres profesor) ["y"])
  print (PruebaS (libres más_joven_xy) ["x", "y"])
  print (PruebaM (sort (libres igual_sumas)) [["a", "b", "b", "c"], ["a", "b", "c"]])
  print (PruebaM (sort (libres igual_sumas_4)) [["1", "1", "1", "1", "4"], ["1", "4"]])
  print (PruebaM (sort (libres axioma)) [["a", "a", "b", "b", "c", "c"], ["a", "b", "c"]])
  print (PruebaS (libres estudiante_profe) [])
  print (PruebaS (libres ej00) ["x"]) -- (∀xP(x))→(∃yP(x))
  print (PruebaS (libres ej01) ["z"]) -- (∀xP(x))∧(∃yP(z))
  print (PruebaM (sort (libres ej02)) [["y", "y", "z"], ["y", "z"]]) -- (∃xP(y,y))↔∃yP(y,z))
  print (PruebaM (sort (libres ej03)) [["g", "g", "x", "z"], ["g", "x", "z"]]) -- ((∃xP(x, g) ∨ ∃yQ(y, g)) → ((∃zR(x) ∧ ¬∀fS(f)) ↔ ∃gT(g, z)))
  print (PruebaS (libres ej04) []) -- ¬∀x(True ∧ ¬∃yFalse)
  print (PruebaS (libres ej05) []) -- ¬∀x∃yTrue

  print "----- ligadas -----"
  print (PruebaS (ligadas (Neg PTrue)) [])
  print (PruebaS (ligadas (Conj PTrue PFalse)) [])
  print (PruebaS (ligadas (Predicado "" [Var "hello_world"])) [])
  print (PruebaS (ligadas igual_ac) [])
  print (PruebaS (ligadas estudiante) [])
  print (PruebaS (ligadas profesor) [])
  print (PruebaS (ligadas más_joven_xy) [])
  print (PruebaS (ligadas igual_sumas) [])
  print (PruebaS (ligadas igual_sumas_4) [])
  print (PruebaS (ligadas axioma) [])
  print (PruebaM (sort (ligadas estudiante_profe)) [["x", "x", "x", "y", "y", "y"], ["x", "y"]])
  print (PruebaM (sort (ligadas ej00)) [["x", "x", "y"], ["x", "y"]]) -- (∀xP(x))→(∃yP(x))
  print (PruebaM (sort (ligadas ej01)) [["x", "x", "y"], ["x", "y"]]) -- (∀xP(x))∧(∃yP(z))
  print (PruebaM (sort (ligadas ej02)) [["x", "y", "y"], ["x", "y"]]) -- (∃xP(y,y))↔∃yP(y,z))
  print (PruebaM (sort (ligadas ej03)) [["f", "f", "g", "g", "x", "x", "y", "y", "z"], ["f", "g", "x", "y", "z"]]) -- ((∃xP(x, g) ∨ ∃yQ(y, g)) → ((∃zR(x) ∧ ¬∀fS(f)) ↔ ∃gT(g, z)))
  print (PruebaS (sort (ligadas ej04)) ["x", "y"]) -- ¬∀x(True ∧ ¬∃yFalse)
  print (PruebaS (sort (ligadas ej05)) ["x", "y"]) -- ¬∀x∃yTrue

  print "----- equiv -----"
  print (PruebaS (equiv (Neg PTrue)) (Neg PTrue))
  print (PruebaS (equiv (Conj PTrue PFalse)) (Conj PTrue PFalse))
  print (PruebaS (equiv (Predicado "" [Var "hello_world"])) (Predicado "" [Var "hello_world"]))
  print (PruebaS (equiv igual_ac) igual_ac)
  print (PruebaS (equiv estudiante) estudiante)
  print (PruebaS (equiv profesor) profesor)
  print (PruebaS (equiv más_joven_xy) más_joven_xy)
  print (PruebaS (equiv igual_sumas) igual_sumas)
  print (PruebaS (equiv igual_sumas_4) igual_sumas_4)
  print (PruebaS (equiv axioma) axioma)
  print (PruebaS (equiv estudiante_profe) estudiante_profe)
  print (PruebaS (equiv ej00) ej00)
  print (PruebaS (equiv ej01) ej01)
  print (PruebaS (equiv ej02) ej02)
  print (PruebaS (equiv ej03) (Impl (Disy (Existe "x" (Predicado "P" [x, g])) (Existe "y" (Predicado "Q" [y, g]))) (Syss (Conj (Existe "z" (Predicado "R" [x])) (Existe "f" (Neg (Predicado "S" [f])))) (Existe "g" (Predicado "T" [g, z])))))
  print (PruebaS (equiv ej04) (Existe "x" (Neg (Conj PTrue (PTodo "y" (Neg PFalse))))))
  print (PruebaS (equiv ej05) (Existe "x" (PTodo "y" (Neg PTrue))))

  print "----- cuantificadores -----"
  print (PruebaS (cuantificadores (Neg PTrue)) 0)
  print (PruebaS (cuantificadores (Conj PTrue PFalse)) 0)
  print (PruebaS (cuantificadores (Predicado "" [Var "hello_world"])) 0)
  print (PruebaS (cuantificadores igual_ac) 0)
  print (PruebaS (cuantificadores estudiante) 0)
  print (PruebaS (cuantificadores profesor) 0)
  print (PruebaS (cuantificadores más_joven_xy) 0)
  print (PruebaS (cuantificadores igual_sumas) 0)
  print (PruebaS (cuantificadores igual_sumas_4) 0)
  print (PruebaS (cuantificadores axioma) 0)
  print (PruebaS (cuantificadores estudiante_profe) 2)
  print (PruebaS (cuantificadores ej00) 2) -- (∀xP(x))→(∃yP(x))
  print (PruebaS (cuantificadores ej01) 2) -- (∀xP(x))∧(∃yP(z))
  print (PruebaS (cuantificadores ej02) 2) -- (∃xP(y,y))↔∃yP(y,z))
  print (PruebaS (cuantificadores ej03) 5) -- ((∃xP(x, g) ∨ ∃yQ(y, g)) → ((∃zR(x) ∧ ¬∀fS(f)) ↔ ∃gT(g, z)))
  print (PruebaS (cuantificadores ej04) 2) -- ¬∀x(True ∧ ¬∃yFalse)
  print (PruebaS (cuantificadores ej05) 2) -- ¬∀x∃yTrue

  print "----- conectivos -----"
  print (PruebaS (conectivos (Neg PTrue)) 1)
  print (PruebaS (conectivos (Conj PTrue PFalse)) 1)
  print (PruebaS (conectivos (Predicado "" [Var "hello_world"])) 0)
  print (PruebaS (conectivos igual_ac) 0)
  print (PruebaS (conectivos estudiante) 0)
  print (PruebaS (conectivos profesor) 0)
  print (PruebaS (conectivos más_joven_xy) 0)
  print (PruebaS (conectivos igual_sumas) 0)
  print (PruebaS (conectivos igual_sumas_4) 0)
  print (PruebaS (conectivos axioma) 1)
  print (PruebaS (conectivos estudiante_profe) 2)
  print (PruebaS (conectivos ej00) 1) -- (∀xP(x))→(∃yP(x))
  print (PruebaS (conectivos ej01) 1) -- (∀xP(x))∧(∃yP(z))
  print (PruebaS (conectivos ej02) 1) -- (∃xP(y,y))↔∃yP(y,z))
  print (PruebaS (conectivos ej03) 5) -- ((∃xP(x, g) ∨ ∃yQ(y, g)) → ((∃zR(x) ∧ ¬∀fS(f)) ↔ ∃gT(g, z)))
  print (PruebaS (conectivos ej04) 3) -- ¬∀x(True ∧ ¬∃yFalse)
  print (PruebaS (conectivos ej05) 1) -- ¬∀x∃yTrue
