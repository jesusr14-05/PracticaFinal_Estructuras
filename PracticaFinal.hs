import Control.Monad (forM, forM_)
import Distribution.Simple.Utils (xargs)
-- López Rojas Jesús
-- Pineda Morales Roberto Gael
-- Practica Final
data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-------------------- EJERCICIO 1 --------------------
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y | y <- xs, y /= x]

variables :: Formula -> [Var]
variables (Atom var) = [var]
variables (Neg formula) = conjunto (variables formula)
variables (formula1 :&: formula2) = conjunto (variables formula1 ++ variables formula2)
variables (formula1 :|: formula2) = conjunto (variables formula1 ++ variables formula2)
variables (formula1 :=>: formula2) = conjunto (variables formula1 ++ variables formula2)
variables (formula1 :<=>: formula2) = conjunto (variables formula1 ++ variables formula2)

-----------------------------------------------------

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom var) = Neg (Atom var)
negacion (Neg formula) = formula
negacion (formula1 :&: formula2) = Neg formula1 :|: Neg formula2
negacion (formula1 :|: formula2) = Neg formula1 :&: Neg formula2
negacion (formula1 :=>: formula2) = negacion (Neg formula1 :|: formula2)
negacion (formula1 :<=>: formula2) = negacion ((formula1 :=>: formula2) :&: (formula2 :=>: formula1))

-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom var) = Atom var
equivalencia (Neg formula) = Neg (equivalencia formula)
equivalencia (formula1 :&: formula2) = equivalencia formula1 :&: equivalencia formula2
equivalencia (formula1 :|: formula2) = equivalencia formula1 :|: equivalencia formula2
equivalencia (formula1 :=>: formula2) = Neg (equivalencia formula1) :|: equivalencia formula2
equivalencia (formula1 :<=>: formula2) = 
    (Neg (equivalencia formula1) :|: equivalencia formula2) :&: (Neg (equivalencia formula2) :|: equivalencia formula1)

-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
--es la interpretcion de una variable en una lista de asignaciones
interpretacionvar :: Var -> [(Var,Bool)] ->Bool
interpretacionvar v ((x, b):xs) = if v == x 
                                    then b 
                                    else interpretacionvar v xs
interpretacionvar _ [] = False

--interpretacion de una formula lógica
interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion (Atom v) xs = interpretacionvar v xs
interpretacion (Neg t) xs = not (interpretacion t xs)
interpretacion (p :&: q) xs = (interpretacion p xs) && (interpretacion q xs)
interpretacion (p :|: q) xs = (interpretacion p xs) || (interpretacion q xs)
interpretacion (p :=>: q) xs = not (interpretacion p xs) || (interpretacion q xs)
interpretacion (p :<=>: q) xs = (interpretacion p xs) == (interpretacion q xs)

-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
agregar :: a -> [[a]] -> [[a]]
agregar x [] = []
agregar x (y:ys) = ((x:y) : agregar x ys)

auxiliar :: [Var] -> [[(Var,Bool)]]
auxiliar [x] = [[(x,True)], [(x,False)]]
auxiliar (x:xs) = (agregar (x,False) (auxiliar xs)) ++ (agregar (x,True) (auxiliar xs))

combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones p = auxiliar(variables p)
-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------

tablaDeVerdadAux :: Formula ->[[(Var,Bool)]] -> [([(Var,Bool)],Bool)]
tablaDeVerdadAux formula [] = []
tablaDeVerdadAux formula (x:xs) = (x, interpretacion formula x)
    :(tablaDeVerdadAux formula xs)

tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad formula = tablaDeVerdadAux formula (combinaciones formula)
-----------------------------------------------------