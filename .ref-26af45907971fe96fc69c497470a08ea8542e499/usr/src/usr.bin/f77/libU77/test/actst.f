C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)actst.f	5.2 (Berkeley) %G%
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
