C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)sigfpe.f	5.2 (Berkeley) %G%
C

	subroutine sigfpe()
	call signal(8, sigfpe, -1)
	return
	end
