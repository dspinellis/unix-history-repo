C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)datst.f	5.2 (Berkeley) %G%
C

	character*24 fdate
	integer id(3), it(3)
	write(*,*) fdate()
	call itime(it)
	call idate(id)
	write(*,*) id, it
	end
