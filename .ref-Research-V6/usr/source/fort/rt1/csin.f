	function csin(z1)
	complex csin, z1, z2
	dimension dummy(2)
	equivalence (a,z2,dummy(1)), (b,dummy(2))

	z2 = z1
	c = exp(b)
	d = 1.0/c
	b = cos(a)*(c-d)/2.0
	a = sin(a)*(c+d)/2.0
	csin = z2
	return
	end
