#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.2 (Berkeley) %G%
#
SRCS=	m.mac m0 m0a m1 m2 m3 m4 m5
APNDX=	table1 table2
MARCOS=	-msU
PRINTER=Pdp
EQN=	deqn -${PRINTER}
TBL=	dtbl -${PRINTER}
TROFF=	ditroff -${PRINTER}

paper:	paper.${PRINTER} apndx.${PRINTER}
	lpr -${PRINTER} -n paper.${PRINTER}
	lpr -${PRINTER} -n apndx.${PRINTER}

paper.${PRINTER}: ${SRCS}
	${EQN} ${SRCS} | ${TBL} | ${TROFF} ${MACROS} -t > paper.${PRINTER}

apndx.${PRINTER}: ${APNDX}
	${EQN} ${APNDX} | ${TBL} | ${TROFF} ${MACROS} -t > apndx.${PRINTER}

clean:
	rm -f paper.${PRINTER} apndx.${PRINTER} *.spell errs Errs make.out

spell:	${SRCS} ${APNDX}
	@for i in ${SRCS} ${APNDX}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
