C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)syml.f	5.2 (Berkeley) %G%
C

	program syml

	integer symlnk
	external symlnk
	write(*,*) symlnk("xyzzy", "poof")
	end
