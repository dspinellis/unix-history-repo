#
# Copyright (c) 1984 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.2 (Berkeley) %G%
#
SRCS=	0.t 1.t 2.t 3.t 4.t
PIC=	pic ${PRINTER}
TBL=	dtbl ${PRINTER}
EQN=	deqn ${PRINTER}
TROFF=	ditroff ${PRINTER}
PRINTER=-Plz
TYPE=	-n

paper:	perf
	lpr ${PRINTER} ${TYPE} perf

perf:	${SRCS} Makefile
	soelim ${SRCS} | ${PIC} | ${TBL} | ${EQN} | ${TROFF} -ms -t > perf
