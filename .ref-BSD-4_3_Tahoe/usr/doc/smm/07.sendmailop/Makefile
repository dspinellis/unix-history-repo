#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	%W% (Berkeley) %G%
#
PRINTER=-Pdp
TROFF=	ditroff ${PRINTER}
EQN=	deqn ${PRINTER}
TBL=	dtbl ${PRINTER}
PIC=	pic ${PRINTER}
LPR=	lpr -n ${PRINTER}

SRC=	op.me

op: op.out
	${LPR} op.out

op.out: ${SRC}
	${PIC} ${SRC} | ${EQN} | ${TROFF} -t -me > op.out

clean:
	rm -f op.out op.me.spell errs Errs

spell:	${SRC}
	@for i in ${SRC}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
