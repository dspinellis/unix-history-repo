#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.1 (Berkeley) %G%
#
#SRCS=	m.mac m0 m0a m1 m2 m3 m4 m5 table1 table2 add obsolete
SRCS=	m.mac m0 m0a m1 m2 m3 m4 m5 table1 table2
MARCOS=	-msU
PRINTER=Pdp
EQN=	deqn -${PRINTER}
TBL=	dtbl -${PRINTER}
TROFF=	ditroff -${PRINTER}

paper:	paper.${PRINTER}
	lpr -${PRINTER} -n paper.${PRINTER}

paper.${PRINTER}: ${SRCS}
	${EQN} ${SRCS} | ${TBL} | ${TROFF} ${MACROS} -t > paper.${PRINTER}

clean:
	rm -f paper.${PRINTER} *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
