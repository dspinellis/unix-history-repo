#
# Copyright (c) 1984 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.1 (Berkeley) %G%
#
SRCS=	0.t 1.t 2.t 3.t 4.t
TBL=	tbl
EQN=	eqn
TROFF=	itroff
TYPE=	-t

paper:	perf
	lpr -Pimagen ${TYPE} perf

perf:	${SRCS} Makefile
	${TBL} ${SRCS} | ${EQN} | ${TROFF} -ms -t > perf
