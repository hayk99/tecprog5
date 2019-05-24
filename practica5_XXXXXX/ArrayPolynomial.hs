{-
	AUTORES: HAYK KOCHARYAN (757715) & JAVIER SALAMERO SANZ (756868)
	PRACTICA 5 TECNOLOGÍA DE LA PROGRAMACIÓN
	FECHA DE MODIFICACIÓN: 24 / 05 / 2019
-}
module ArrayPolynomial where

type Polinomio = [Float]

--definir funciones
 
--Fucion que devuelve polinomio de grado 1
x :: Polinomio
x = [1,0]

--Funcion que devuelve polinomio de grado 0
coef :: Float -> Polinomio
coef c = [c]

--Funcion auxiliar para sumarLength
zipWith' ::  Polinomio -> Polinomio -> Int -> Int -> Polinomio
zipWith' p1 p2 longp1 longp2 = if (longp1 > longp2)
								then [head p1] ++ zipWith' (tail p1) p2 (longp1-1) longp2
							   else zipWith (+) p1 p2

--Funcion auxiliar, aplicando inmersion, que dado dos polinomios y sus longitudes correspondientes devuleve 
--la suma de estos dos
sumarLength :: Polinomio -> Int -> Polinomio -> Int -> Polinomio
sumarLength p1 longp1 p2 longp2 = if (longp1 >= longp2)
								  		--then zipWith (+) p1 p2
								  		then zipWith' p1 p2 longp1 longp2
								  		--else sumarLength p2 longp2 (headp1:p1) longp1
								  		else zipWith' p2 p1 longp2 longp1

--funcion que dado dos polinomios, devuelve la suma de estos dos
sumar :: Polinomio -> Polinomio -> Polinomio
sumar [] p2 = p2;
sumar p1 [] = p1;
sumar p1 p2 = sumarLength p1 (length(p1)) p2 (length(p2))


--Funcion que dado una lista de polinomios, devuelve la suma de estos
padd :: [Polinomio] -> Polinomio
padd [] = []
padd (x:[]) = x
padd (x:xs) = sumar x (padd xs)

{-
--Eliminamos esta funcion y la incluimos en el let de la funcon multiplicar, 
de esta forma elimnamos una función y la reducimos a una linea.

Funcion que multiplica un numero por un polinomio
multSimple :: Float -> Polinomio -> Polinomio
multSimple num pol = map (*num) pol
-}

--concatena un 0 para tener el polinomio adecuado para sumar a la hora de multiplicar
multiplicarAntigua :: Polinomio -> Polinomio
multiplicarAntigua p = p ++ [0]

--Funcion que dado dos polinomios, los multiplica
multiplicar :: Polinomio -> Polinomio -> Polinomio
multiplicar [] p2 = []
multiplicar p1 [] = []
multiplicar p1 p2 = let 
							--multiplicamos como en el cole, desde las unidades
							--pVecesP2 = multSimple (last p1) p2
							pVecesP2 = map (*(last p1)) p2
							--recursividad quitando la cifra multiplicada antes
							multResto = multiplicarAntigua ( multiplicar (init p1) p2 )
						in 
							sumar pVecesP2 multResto

--Funcion que dada una lista de varios polinomios, los multiplica y devuelve un polinomio.
pmul :: [Polinomio] -> Polinomio
pmul [] = []
pmul (x:[]) = x
pmul (x:xs) = multiplicar x (pmul xs)

--Funcion que dado un polinomio y el valor de 'x', devuelve el resultado de evaluar el polinomio.
peval :: Polinomio -> Float -> Float
peval [] a = 0.0
peval p num = (last p) + peval ( init (map (*num) p) ) num

--Funcion auxiliar para calcular la derivada de un polinomio dado este y su longitud.
calcularDerivada :: Polinomio -> Int -> Polinomio
calcularDerivada [] _ = []
calcularDerivada (x:restoPol) tam = [x*(fromIntegral(tam))] ++ calcularDerivada restoPol (tam-1)

--Función que calcula la derivada de un polinomio
pderv :: Polinomio -> Polinomio
pderv [] = []
pderv p = calcularDerivada (init p) (length(p)-1)