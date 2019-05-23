--import ArrayPolynomial
import TupleListPolynomial

main = do 
		print p
		print p1
		print p2
		print p3
		print p4
		print p5
		--print p1
		--print p2 	
		--print "           "
		--print p4
		--print p5
		--print p6
	--	print (peval p  2.0)
	--	print dp
	--	print (peval dp 2.0)
		where
			--p  = padd [(pmul [x,x]),(coef 3.0),(pmul [(coef 2.0),x])]
			p = x
			p1 = coef 5.0
			p2 = peval [(3,4),(2,3),(1,2)] 2
			p3 = pderv [(3,4),(2,3),(1,2),(8,1),(5,0)]
			p4 = padd [ [(2,3),(1,2)], [(3,3),(5,2)], [(4,4),(4,3),(1,2)], [(3,3),(5,2)]]
			p5 = pmul [ [(2,3),(5,2)], [(2,3),(5,2)], [(0.1,3)]] 



