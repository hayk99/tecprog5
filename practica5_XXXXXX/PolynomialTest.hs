import ArrayPolynomial
--import TupleListPolynomial

main = do 
		print p
		print p1
		print p2 	
		print p3
		print "           "
		print p4
		print p5
	--	print (peval p  2.0)
	--	print dp
	--	print (peval dp 2.0)
		where
			--p  = padd [(pmul [x,x]),(coef 3.0),(pmul [(coef 2.0),x])]
			p = padd [ [2,2,6], [2,3,5,5] ]
			p1 = multSimple  2 [2,4,6,8]
			p2 = multiplicar [2,3] [1,2,3]
			--p3 = multSimple 2 [1,2,3]
			p3 = pmul [ [2,1], [4,2], [2,2]]
			p4 = peval [1,2,3] 4
			p5 = pderv [1,2,3]
			--dp = pderv p



