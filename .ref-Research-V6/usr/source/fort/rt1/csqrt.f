	function csqrt(z1)
	complex csqrt, z1, z2
	dimension dummy(2)
	equivalence (a,z2,dummy(1)), (b,dummy(2))

	z2 = z1
	c = cabs(z2)
	c = sqrt(c)
	b = atan2(b, a)/2.0
	a = c*cos(b)
	b = c*sin(b)
	csqrt = z2
	return
	end
