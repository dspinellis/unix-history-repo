C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)rnmtst.f	5.2 (Berkeley) %G%
C

	program rnmtst
	integer rename
	external rename

	open(1, file="abc")
	close(1)
	call system ("ls -l abc def")
	write(*,*) "rename:", rename("abc", "def")
	call system ("ls -l abc def")
	end
