	function clog(z1)
	complex clog, z1, z2
	dimension dummy(2)
	equivalence (a,z2,dummy(1)), (b,dummy(2))

	z2 = z1
	c = cabs(z2)
	b = atan2(b, a)
	a = alog(c)
	clog = z2
	return
	end
