C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)sigtrtst.f	5.2 (Berkeley) %G%
C

	integer signal
	external trap
	isav = signal(8, 0, 1)
	write(*,*) "1", signal(8, 0, 0)
	write(*,*) "0", signal(8, trap, -1)
	write(*,*) "addr", signal(8, 0, isav)
	write(*,*) isav, signal(8, trap, -1)
	x = 1.0/x
	end

	subroutine trap(isig)
	write(*,*) "Here I am."
	end
