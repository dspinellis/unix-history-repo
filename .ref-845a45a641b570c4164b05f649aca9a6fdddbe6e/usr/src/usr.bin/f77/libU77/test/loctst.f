C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)loctst.f	5.2 (Berkeley) %G%
C

	common/blah/i,j,k
	double precision d
	equivalence (d,i),(x,i)
	write(*,*) loc(i), loc(d), loc(x), loc(z)
	end
