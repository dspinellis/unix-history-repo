C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C This module is believed to contain source code proprietary to AT&T.
C Use and redistribution is subject to the Berkeley Software License
C Agreement and your Software Agreement with AT&T (Western Electric).
C
C	@(#)pe.f	5.2 (Berkeley) 4/12/91
C

	program pe
	x = 123.456
	write(*,'(3pe8.1,5x,0pe9.5e0)') x, 123456789e14
	end
