--import ArrayPolynomial
import TupleListPolynomial

main = do 
		print p
		print p1
		print p2
		print p3
		print p4
		print "p5____________________-"
		print p5
		--print p1
		--print p2 	
		--print "           "
		--print p4
		--print p5
		print p6
		print p7
		--multiplicar 2x³ * (5x³ + 2x³ )
		print p8
		print p9
		--multiplicar tocha con 3 
		print "p10 tocha_____________"
		print p10
	--	print (peval p  2.0)
	--	print dp
	--	print (peval dp 2.0)
		where
			--p  = padd [(pmul [x,x]),(coef 3.0),(pmul [(coef 2.0),x])]
			p = x
			--pol = [3,0,2,1]
			--pol1 = [1,1,1,1]

			--p1 = padd [ pol,pol1]
			p1 = coef 5.0
			p2 = peval [(3,4),(2,3),(1,2)] 2
			p3 = pderv [(3,4),(2,3),(1,2),(8,1),(5,0)]
			p4 = padd [ [(2,3),(1,2)], [(3,3),(5,2)], [(4,4),(4,3),(1,2)], [(3,3),(5,2)]]
			--p5 = pmul [ [(2,3),(5,2)], [(2,3),(5,2)] ] 
			--p6 = pmul [ [(2,4),(5,2)], [(2,3),(5,2)] ] 
			p5 = bases 2 [5,2]
			p6 = exponentes 3 [3,2]
			p7 = zip p5 p6
			p8 = mult [(2,3)] [(5,3),(2,2)]
			p9 = mult [(2,4),(3,2)] [(5,3),(2,2)]
			p10 = pmul [ [(2,2),(3,0)], [(3,3),(2,2),(1,1)], [(5,2), (5,1)]]



