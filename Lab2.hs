module Lab2 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Nicolas Piriz y Manuel Stapff
-- Números: 310896 y 303636
----------------------------------------------------

import Prelude
import Data.List (nub)

-- Formalización del lenguaje
type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)
data BC = And | Or | Imp | Iff
  deriving (Eq)

-- Fórmulas del Lab1
p = V "p"
q = V "q"
r = V "r"
fa :: L
fa = Bin p And (Neg (Neg q))                   -- (p ∧ ¬¬q)
fb :: L
fb = Bin p And (Bin (Neg q) And (Neg r))       -- (p ∧ ¬q ∧ ¬r)
fc :: L
fc = Bin (Neg (Neg p)) Or (Neg (Bin q And p))  -- (¬¬p ∨ ¬(q ∧ p))
fd :: L
fd = Bin (Neg (Bin r Imp r)) And fc            -- ¬(r ⊃ r) ∧ (¬¬p ∨ ¬(q ∧ p))


-- EJERCICIO 1 --
--1.1)
eval :: (Var -> Bool) -> L -> Bool
eval i (V x) = i (x)
eval i (Neg f) = not (eval i f)
eval i (Bin x op y) | op == And = (eval i x) && (eval i y)
                    | op == Or = (eval i x) || (eval i y)
                    | op == Imp = (eval i x) <= (eval i y) 
                    | op == Iff = (eval i x) == (eval i y)


--1.2)
itodasverdaderas ::  Var -> Bool
itodasverdaderas (var) = True

--1.3)
itodasfalsas :: Var -> Bool
itodasfalsas (var) = False

--1.4)
irfalsa :: Var -> Bool
irfalsa ("r") = False
irfalsa (x) = True

--1.5)
-- Completar con verdadera/falsa:
-- fa es falsa bajo itodasfalsas
-- fb es falsa bajo itodasfalsas
-- fc es verdadera bajo itodasfalsas
-- fd es falsa bajo itodasfalsas
-- 
-- fa es verdadera bajo itodasverdaderas
-- fb es falsa bajo itodasverdaderas
-- fc es verdadera bajo itodasverdaderas
-- fd es falsa bajo itodasverdaderas
--
-- fa es verdadera bajo irfalsa
-- fb es falsa bajo irfalsa
-- fc es verdadera bajo irfalsa
-- fd es falsa bajo irfalsa

--1.6)
creari :: [(Var, Bool)] -> (Var -> Bool)
creari [] = error "Variable no encontrada"
creari ((var, bool): xs) = \x -> if x == var then bool 
                                              else (creari xs) x

--1.7)
--La interpretacion es igual siempre y cuando se den las siguientes condiciones:
--1.La cantidad de variables en la interpretacion del punto 4 sea la misma
--2.r, p y q esten presentes en dicha interpretacion y sus valores booleanos sean False, True y True respectivamente


-- EJERCICIO 2 --
type Fila = [(Var, Bool)]
type TV = [(Fila, Bool)]

data Clase = Tau | Contra | Cont | Sat | Fal

--2.1)
filas :: [Var] -> [Fila]
filas [] = [[]]
filas (x:xs) = [[(x, b)] ++ vs | b <- [True, False], vs <- filas xs]

--2.2)
-- se usa la funcion vars de practico 1
-- con vars agarras las variables, despues creas las filas posibles para esas variable
-- para cada una de estas filas, se crea un i. Despues se hace eval de la funcion con ese i
listarProp :: L -> [Var]
listarProp (V x) = [x]
listarProp (Neg form) = nub (listarProp form)
listarProp (Bin a x b) = nub ((listarProp a) ++ (listarProp b)) 

resultadosFilas :: [Fila] -> L -> TV
resultadosFilas [] formula = []
resultadosFilas (x:filas) formula = (x, eval (creari x) formula): resultadosFilas filas formula

tv :: L -> TV
tv formula = resultadosFilas (filas (listarProp formula)) formula


--2.3)

resultados :: L -> [Bool]
resultados formula = [snd x | x <- tv formula]

esTau :: [Bool] -> Bool
esTau [] = True
esTau (bool:bools) = if bool == True then esTau bools
                                  else False

esContra :: [Bool] -> Bool
esContra [] = True
esContra (bool:bools) = if bool == False then esContra bools
                                      else False

esSat :: [Bool] -> Bool
esSat [] = False
esSat (bool:bools) = if bool == True then True
                                  else esSat bools

esCont :: [Bool] -> Bool
esCont lista = not (esTau lista) && not (esContra lista)

esFal :: [Bool] -> Bool
esFal lista = (esCont lista) || (esContra lista)


es :: L -> Clase -> Bool
es formula Tau = esTau (resultados formula)
es formula Contra = esContra (resultados formula)
es formula Sat = esSat (resultados formula)
es formula Cont = esCont (resultados formula)
es formula Fal = esFal (resultados formula)

--2.4)
-- Completar con tautología/contingencia/contradicción:
-- fa es una contingencia
-- fb es una contingencia
-- fc es una tautología
-- fd es una contradicción

--2.5)
modelosFalsos :: TV -> [Fila]
modelosFalsos [] = []
modelosFalsos ((fila, bool):xs) = if bool == False then [fila] ++ modelosFalsos(xs)
                                                   else modelosFalsos(xs)

negarTrues :: Fila -> [L]
negarTrues [] = []
negarTrues ((var, bool):xs) = if bool == True then [Neg(V var)] ++ negarTrues(xs)
                                              else [V var] ++ negarTrues(xs)

disyuncionesFnc :: [L] -> L
disyuncionesFnc [] = Bin (V "x") Or (Neg (V "x"))
disyuncionesFnc [x] = x
disyuncionesFnc (x:xs) = Bin x Or (disyuncionesFnc (xs))

conjuncionesFnc :: [L] -> L
conjuncionesFnc [] = Bin (V "x") Or (Neg (V "x"))
conjuncionesFnc [x] = x
conjuncionesFnc (x:xs) = Bin x And (conjuncionesFnc (xs)) 

fnc :: L -> L
fnc form = conjuncionesFnc (map (disyuncionesFnc) (map (negarTrues) (modelosFalsos (tv (form)))))
----------------------------------------------------------------------------------
-- Pretty Printing (rudimentario)
----------------------------------------------------------------------------------
instance Show L where
  show (V p)         = p
  show (Neg (Neg a)) = "¬" ++ show (Neg a)
  show (Neg (V p))   = "¬ " ++ show (V p)
  show (Neg a)       = "¬ (" ++ show a ++ ")"
  show (Bin a And b) = "(" ++ show a ++ ") /\\ (" ++ show b ++ ")"
  show (Bin a Or b)  = "(" ++ show a ++ ") \\/ (" ++ show b ++ ")"
  show (Bin a Imp b) = "(" ++ show a ++ ") --> (" ++ show b ++ ")"
  show (Bin a Iff b) = "(" ++ show a ++ ") <-> (" ++ show b ++ ")"