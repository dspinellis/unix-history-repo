	function dccos(z1)
	double complex dccos, z1, z2
	double precision dummy(2), a, b, c, d
	equivalence (a,z2,dummy(1)), (b,dummy(2))

	z2 = z1
	c = dexp(b)
	d = 1.0d0/c
	b = dsin(a)*(d-c)/2.0d0
	a = dcos(a)*(c+d)/2.0d0
	dccos = z2
	return
	end
