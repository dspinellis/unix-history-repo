	function ccos(z1)
	complex ccos, z1, z2
	dimension dummy(2)
	equivalence (a,z2,dummy(1)), (b,dummy(2))

	z2 = z1
	c = exp(b)
	d = 1.0/c
	b = sin(a)*(d-c)/2.0
	a = cos(a)*(c+d)/2.0
	ccos = z2
	return
	end
