	function dcsqrt(z1)
	double complex dcsqrt, z1, z2
	double precision dummy(2), a, b, c
	equivalence (a,z2,dummy(1)), (b,dummy(2))

	z2 = z1
	c = dcabs(z2)
	c = dsqrt(c)
	b = datan2(b, a)/2.0d0
	a = c*dcos(b)
	b = c*dsin(b)
	dcsqrt = z2
	return
	end
