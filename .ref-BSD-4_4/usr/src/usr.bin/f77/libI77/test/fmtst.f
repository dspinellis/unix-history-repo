C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C This module is believed to contain source code proprietary to AT&T.
C Use and redistribution is subject to the Berkeley Software License
C Agreement and your Software Agreement with AT&T (Western Electric).
C
C	@(#)fmtst.f	5.2 (Berkeley) 4/12/91
C

	real x(8)
	do 10 i=1,8
   10	x(i) = i
	write(*,1000) (i,x(i),i=1,8)
 1000	format (2("x(",i,")= ",f3.0," feet, "), "ta da")
	end
