C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C This module is believed to contain source code proprietary to AT&T.
C Use and redistribution is subject to the Berkeley Software License
C Agreement and your Software Agreement with AT&T (Western Electric).
C
C	@(#)argtst.f	5.2 (Berkeley) 4/12/91
C

	character*16 arg, env
	n = iargc()
	call getarg(0, arg)
	write(*,*) "name:", arg
	if (n .gt. 0) then
	  do 100 i=1, n
	  call getarg(i, arg)
	  call getenv(arg, env)
  100	  write(*,*) i, arg, env
	endif
	end
