#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.2 (Berkeley) 5/2/86
#
SRCS=	0.t 1.t 2.t 3.t 4.t 5.t
MACROS=	-ms
PRINTER=-Pdp
EQN=	deqn ${PRINTER}
TBL=	dtbl ${PRINTER}
TROFF=	ditroff ${PRINTER}

ipc:	ipc.${PRINTER}
	lpr ${PRINTER} -n ipc.${PRINTER}

ipc.${PRINTER}: ${SRCS}
	${TBL} ${SRCS} | ${TROFF} ${MACROS} -t > ipc.${PRINTER}

clean:
	rm -f ipc.-* *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
