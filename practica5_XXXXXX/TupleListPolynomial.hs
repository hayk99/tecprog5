{-
	AUTORES: HAYK KOCHARYAN (757715) & JAVIER SALAMERO SANZ (756868)
	PRACTICA 5 TECNOLOGÍA DE LA PROGRAMACIÓN
	FECHA DE MODIFICACIÓN: 24 / 05 / 2019
-}
module TupleListPolynomial where

type TupleList = [(Float, Integer)]

--definir funciones	

--Fucion que devuelve polinomio de grado 1
x :: TupleList
x =  [ (1.0, 1) ]

--------------------------------------------------------------------------------------
--Funcion que devuelve polinomio de grado 0
coef :: Float -> TupleList
coef a = [(a, 0)]

--------------------------------------------------------------------------------------
--Funcion que evalua un polinomio, dado el valor de x
peval :: TupleList -> Float -> Float
peval [] _ = 0
peval (a:xs) n = ( (n^snd(a))*fst(a) ) + (peval xs n)

--------------------------------------------------------------------------------------
--Funcion que calcula la derivada de un polinomio
pderv :: TupleList -> TupleList
pderv [] = []
pderv (a:xs) = 
	if snd(a) == 0 then []
	else [ (fst(a)*fromInteger(snd(a)), snd(a)-1) ] ++ pderv xs

--------------------------------------------------------------------------------------
--Funcion que dado una lista de polinomios, devuelve la suma de todas estas
padd :: [TupleList] -> TupleList
padd [] = []
padd (x:[]) = x
padd (a:xs) = suma a (padd xs)

--Funcion que dado dos tuplas, devuelve la suma de estas
suma :: TupleList -> TupleList -> TupleList
suma [] p2 = p2
suma p1 [] = p1
suma p1 p2 = 
	--si la tupla a1 es de mayor grado que la tupa a2
	if (snd(head p1)) > (snd(head p2)) 
		--entonces cojo el primero de a1 y sumo lo demás
		then [ (head p1) ] ++ suma (tail p1) p2
	else if (snd(head p1)) < (snd(head p2))
		then [ (head p2) ] ++ suma p1 (tail p2)
	else [ ( (fst(head p1) + fst(head p2)), snd(head p2) )] ++ suma (tail p1) (tail p2)

--------------------------------------------------------------------------------------
--devuelve la lista resultante de multiplicar la base de uno de los elementos
--de un polinomio por las bases de otro polinomio
bases :: Float -> [Float] -> [Float]
bases flo basesPoli = map (flo*) basesPoli

--devuelve la lista resultante de multiplicar el exponente de uno de los elementos
--de un polinomio por los exponentes de otro polinomio
exponentes :: Integer -> [Integer] -> [Integer]
exponentes exp expPoli = map (exp+) expPoli

--junta las dos listas generadas por la suma y multiplicación de 
--los exponentes y las bases, respectivamente.
mult2 :: TupleList -> TupleList -> TupleList
mult2 ((a,b):p2s) p2 = zip (bases a (map fst p2)) (exponentes b (map snd p2))

--Dado dos polinomios, devuelve la multiplicación de estas
mult :: TupleList -> TupleList -> TupleList
mult [] p2 = []
mult p1 [] = []
mult p1 p2 = suma (mult2 ([head(p1)]) p2) (mult (tail(p1)) p2)

--Devuelve el Polinomio resultante de multiplicar una lista con 1 o más polinomios
pmul :: [TupleList] -> TupleList
pmul [] = []
pmul (x:[]) = x
pmul (a:xs) = mult a (pmul xs)


--base, exponente