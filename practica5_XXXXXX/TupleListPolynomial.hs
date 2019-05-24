module TupleListPolynomial where

type TupleList = [(Float, Integer)]

--definir funciones	

x :: TupleList
x =  [ (1.0, 1) ]

-------------------------------------------------------
coef :: Float -> TupleList
coef a = [(a, 0)]

-------------------------------------------------------
--Funcion que evalua una ecuación dado el valor de x
peval :: TupleList -> Float -> Float
peval [] _ = 0
peval (a:xs) n = ( (n^snd(a))*fst(a) ) + (peval xs n)

-------------------------------------------------------
--Funcion que calcula la derivada de una lista de tuplas
pderv :: TupleList -> TupleList
pderv [] = []
pderv (a:xs) = 
	if snd(a) == 0 then []
	else [ (fst(a)*fromInteger(snd(a)), snd(a)-1) ] ++ pderv xs

-------------------------------------------------------
padd :: [TupleList] -> TupleList
padd [] = []
padd (x:[]) = x
padd (a:xs) = suma a (padd xs)

suma :: TupleList -> TupleList -> TupleList
suma [] b = b
suma a [] = a
suma a b = 
	if (snd(head a)) > (snd(head b)) 
		then [ (head a) ] ++ suma (tail a) b
	else if (snd(head a)) < (snd(head b))
		then [ (head b) ] ++ suma a (tail b)
	else [ ( (fst(head a) + fst(head b)), snd(head b) )] ++ suma (tail a) (tail b)

-------------------------------------------------------
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

mult :: TupleList -> TupleList -> TupleList
mult [] p2 = []
mult p1 [] = []
mult p1 p2 = suma (mult2 ([head(p1)]) p2) (mult (tail(p1)) p2)

pmul :: [TupleList] -> TupleList
pmul [] = []
pmul (x:[]) = x
pmul (a:xs) = mult a (pmul xs)


--base, exponente