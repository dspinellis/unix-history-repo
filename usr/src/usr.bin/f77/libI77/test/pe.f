C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)pe.f	5.2 (Berkeley) %G%
C

	program pe
	x = 123.456
	write(*,'(3pe8.1,5x,0pe9.5e0)') x, 123456789e14
	end
