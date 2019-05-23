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
pmul :: [TupleList] -> TupleList
pmul [] = []
pmul (x:[]) = x
pmul (a:xs) = mult a (padd xs)

mult :: TupleList -> TupleList -> TupleList
mult [] b = b
mult a [] = a
mult a b = 
	if (snd(head a)) > (snd(head b)) 
		then [ (head a) ] ++ mult (tail a) b
	else if (snd(head a)) < (snd(head b))
		then [ (head b) ] ++ mult a (tail b)
	else [ ( (fst(head a) * fst(head b)), snd(head b) )] ++ mult (tail a) (tail b)
