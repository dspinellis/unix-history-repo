C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)gwdtst.f	5.2 (Berkeley) %G%
C

	character*50 path
	integer getcwd, chdir

	ier = getcwd(path)
	write(*,*) ier, path
	ier = chdir("..")
	ier = getcwd(path)
	write(*,*) ier, path
	end
