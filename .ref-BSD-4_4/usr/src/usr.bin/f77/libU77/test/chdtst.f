C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C This module is believed to contain source code proprietary to AT&T.
C Use and redistribution is subject to the Berkeley Software License
C Agreement and your Software Agreement with AT&T (Western Electric).
C
C	@(#)chdtst.f	5.2 (Berkeley) 4/12/91
C

	integer chdir, unlink, access, ier(8)
	write(*,*) "moving to /tmp"
	ier(1) = chdir("/tmp")
	call system("pwd")
	open(1, file="scratch")
	close(1)
	write(*,*) "linking"
	ier(2) = link("scratch", "temp")
	ier(3) = access("scratch", " ")
	ier(4) = access("temp", " ")
	call system("ls -il scratch temp")
	write(*,*) "unlinking"
	ier(5) = unlink("scratch")
	ier(6) = unlink("temp")
	ier(7) = access("scratch", " ")
	ier(8) = access("temp", " ")
	call system("ls scratch temp")
	write(*,*) ier
	end
