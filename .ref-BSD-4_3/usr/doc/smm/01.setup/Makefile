#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.1 (Berkeley) 5/13/86
#
PRINTER=-Pdp
TROFF=	ditroff ${PRINTER}
EQN=	deqn ${PRINTER}
TBL=	dtbl ${PRINTER}
LPR=	lpr -n ${PRINTER}

SRC=	0.t 1.t 2.t 3.t 4.t 5.t 6.t a.t b.t c.t

setup: setup.out
	${LPR} setup.out

setup.out: ${SRC}
	${EQN} ${SRC} | ${TBL} | ${TROFF} -t -msU > setup.out

clean:
	rm -f setup.out ?.t.spell errs Errs

spell:	${SRC}
	@for i in ${SRC}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
