	function dclog(z1)
	double complex z1, z2, dclog
	double precision a,b,c
	double precision dummy(2)
	equivalence (a,z2,dummy(1)), (b,dummy(2))

	z2=z1
	c=dcabs(z2)
	b=datan2(b,a)
	a=dlog(c)
	dclog=z2
	return
	end
