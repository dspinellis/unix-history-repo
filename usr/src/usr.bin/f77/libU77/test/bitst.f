C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)bitst.f	5.2 (Berkeley) %G%
C

	logical bit
	i = 0
	call bis(30, i)
	write(*,*) i, bit(30, i), bit(4, i)
	call bic(30, i)
	write(*,*) i, bit(0, i)
	call bis(0, i)
	write(*,*) i
	end
