	function dcsin(z1)
	double complex dcsin, z1, z2
	double precision dummy(2), a, b, c, d
	equivalence (a,z2,dummy(1)), (b,dummy(2))

	z2 = z1
	c = dexp(b)
	d = 1.0d0/c
	b = dcos(a)*(c-d)/2.0d0
	a = dsin(a)*(c+d)/2.0d0
	dcsin = z2
	return
	end
