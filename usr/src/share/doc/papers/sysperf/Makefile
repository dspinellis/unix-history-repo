#
# Copyright (c) 1985 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.3 (Berkeley) %G%
#
PRINTER=-Plz
TYPE=	-n
SRCS=	0.t 1.t 2.t 3.t 4.t 5.t 6.t 7.t
OBJS=	perf.t appendix.t
TBL=	dtbl
EQN=	deqn
TROFF=	ditroff
GRIND=	vgrind

paper:	perf
	lpr ${PRINTER} ${TYPE} perf

perf:	${OBJS}
	${TROFF} -ms -t ${PRINTER} ${OBJS} > perf

perf.t:	${SRCS}
	${TBL} ${PRINTER} ${SRCS} | ${EQN} ${PRINTER} > perf.t

appendix.t: a1.t a2.t
	${GRIND} -f a1.t |  awk '/\.\(\)/{ cnt = 2 } \
	   { if (cnt) cnt -= 1; else print $$0; } ' > appendix.t
	${GRIND} -f -lcsh a2.t |  awk '/\.\(\)/{ cnt = 2 } \
	   { if (cnt) cnt -= 1; else print $$0; } ' >> appendix.t

clean:
	rm -f perf ${OBJS}
