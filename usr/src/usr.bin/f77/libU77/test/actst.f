C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C This module is believed to contain source code proprietary to AT&T.
C Use and redistribution is subject to the Berkeley Software License
C Agreement and your Software Agreement with AT&T (Western Electric).
C
C	@(#)actst.f	5.2 (Berkeley) 4/12/91
C

	integer access
	write(*,*) "exist?", access("actst.f", " ")
	write(*,*) "execute?", access("actst.f", "x")
	write(*,*) "read?", access("actst.f", "r")
	write(*,*) "write?", access("actst.f", "w")
	write(*,*) "read/write?", access("actst.f", "rw")
	write(*,*) "read/write/exec?", access("actst.f", "rwx")
	write(*,*) "null name?", access(" ", "w")
	write(*,*) "bad arg?", access("actst.f", "zzz")
	write(*,*) "not exist?", access("XQIT%0XGE", " ")
	end
