C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C This module is believed to contain source code proprietary to AT&T.
C Use and redistribution is subject to the Berkeley Software License
C Agreement and your Software Agreement with AT&T (Western Electric).
C
C	@(#)trunctst.f	5.2 (Berkeley) 4/12/91
C

	program trutst
	integer ftell
	external ftell

	rewind 1
	write(1,*) "This is line A."
	write(1,*) "This is line B."
	write(1,*) "This is line C."
	write(1,*) "This is line D."
	backspace 1
	endfile 1
	call system ("cat fort.1")
	write(*,*) "---"
	rewind 1
	write(1,*) "This is line E."
	write(1,*) "This is line F."
	close(1)
	call system ("cat fort.1")
	end
