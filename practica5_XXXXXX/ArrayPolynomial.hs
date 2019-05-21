module ArrayPolynomial where

type Polinomio = [Float]

--definir funciones
 
x :: Polinomio
x = [1,0]

coef :: Float -> Polinomio
coef c = [c]

--funcion que dado dos polinomios, devuelve la suma de estos
sumar :: Polinomio -> Polinomio -> Polinomio
sumar [] p2 = p2;
sumar p1 [] = p1;
--sumar p1 p2 = [(head p1) + (head p2)] ++ (sumar (tail p1) (tail p2))
sumar p1 p2 = if (length p1 >= length p2)
			  then zipWith (+) p1 (p2 ++ repeat 0)
			  else zipWith (+) (p1 ++ repeat 0) p2

--funcion que dado una lista de polinomios, devuelve la suma de estos
padd :: [Polinomio] -> Polinomio
--padd lp
padd [] = []
padd (x:[]) = x
padd (x:xs) = sumar x (padd xs)

--Funcion que multiplica un numero por un polinomio
multSimple :: Float -> Polinomio -> Polinomio
multSimple num pol = map (*num) pol

--concatena un 0 para tener el polinomio adecuado para sumar
multiplicarAntigua p = 0:p

--Funcion que dado dos polinomios, los multiplica
multiplicar :: Polinomio -> Polinomio -> Polinomio
multiplicar [] p2 = []
multiplicar p1 [] = []
multiplicar (p:p1) p2 = let 
							--multiplicamos como en el cole, desde las unidades
							pVecesP2 = multSimple p p2
							--recursividad quitando la cifra multiplicada antes
							multResto = multiplicarAntigua ( multiplicar p1 p2 )
						in 
							sumar pVecesP2 multResto

--Funcion que dada una lista de varios polinomios, los multiplica
pmul :: [Polinomio] -> Polinomio
pmul [] = []
pmul (x:[]) = x
pmul (x:xs) = multiplicar x (pmul xs)

peval :: Polinomio -> Float -> Float
peval [] a = 0.0
peval p num = (last p) + peval ( init (map (*num) p) ) num

pderv :: Polinomio -> Polinomio
pderv [] = []
pderv p = zipWith (*) (init p) [((length p)*1.0)..0.0]