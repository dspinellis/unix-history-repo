C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)sigtst.f	5.2 (Berkeley) %G%
C

	external fpe
	integer signal
	isave = signal(8, 0, 1)
	write(*,*) "1", signal(8, 0, 0)
	write(*,*) "0", signal(8, fpe, -1)
	write(*,*) "addr fpe", signal(8, 0, isave)
	write(*,*) isave, signal(8, fpe, -1)
	x = 1/x
	write(*,*) "Back again"
	z = 1/z
	write(*,*) "Saved again"
	end

	subroutine fpe(flag)
	call signal(8, fpe, -1)
c	call sigset()
	write(*,*) "Hi there sports fans!"
	call flush(6)
	return
	end

	subroutine sigset()
	external fpe
	call signal(8, fpe, -1)
	return
	end
