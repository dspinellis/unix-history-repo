C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C This module is believed to contain source code proprietary to AT&T.
C Use and redistribution is subject to the Berkeley Software License
C Agreement and your Software Agreement with AT&T (Western Electric).
C
C	@(#)datst.f	5.2 (Berkeley) 4/12/91
C

	character*24 fdate
	integer id(3), it(3)
	write(*,*) fdate()
	call itime(it)
	call idate(id)
	write(*,*) id, it
	end
