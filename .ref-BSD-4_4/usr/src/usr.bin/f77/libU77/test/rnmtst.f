C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C This module is believed to contain source code proprietary to AT&T.
C Use and redistribution is subject to the Berkeley Software License
C Agreement and your Software Agreement with AT&T (Western Electric).
C
C	@(#)rnmtst.f	5.2 (Berkeley) 4/12/91
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
