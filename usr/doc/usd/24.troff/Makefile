#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.3 (Berkeley) 4/21/86
#
INTRO=	m0
TUTOR=	m0a
PAPER=	m1 m2 m3 m4
APNDX1=	m5
APNDX2=	table1
APNDX3=	table2
MACROS=	m.mac
PRINTER=Pdp
TBL=	dtbl -${PRINTER}
TROFF=	ditroff -${PRINTER}
LPR=	lpr -n -${PRINTER}

paper:	intro.${PRINTER} tutor.${PRINTER} paper.${PRINTER}
paper:	apndx1.${PRINTER} apndx2.${PRINTER} apndx3.${PRINTER} 
	${LPR} intro.${PRINTER}
	${LPR} tutor.${PRINTER}
	${LPR} paper.${PRINTER}
	${LPR} apndx1.${PRINTER}
	${LPR} apndx2.${PRINTER}
	${LPR} apndx3.${PRINTER}

intro.${PRINTER}: ${INTRO}
	${TROFF} -t ${MACROS} ${INTRO} > intro.${PRINTER}

tutor.${PRINTER}: ${TUTOR}
	${TBL} ${TUTOR} | ${TROFF} -t ${MACROS} - > tutor.${PRINTER}

paper.${PRINTER}: ${PAPER}
	${TBL} ${PAPER} | ${TROFF} -t ${MACROS} - > paper.${PRINTER}

apndx1.${PRINTER}: ${APNDX1}
	${TROFF} -t ${MACROS} ${APNDX1} > apndx1.${PRINTER}

apndx2.${PRINTER}: ${APNDX2}
	${TROFF} -t ${MACROS} ${APNDX2} > apndx2.${PRINTER}

apndx3.${PRINTER}: ${APNDX3}
	${TROFF} -t ${MACROS} ${APNDX3} > apndx3.${PRINTER}

clean:
	rm -f intro.${PRINTER} tutor.${PRINTER} paper.${PRINTER} \
		apndx1.${PRINTER} apndx2.${PRINTER} apndx3.${PRINTER} \
		*.spell errs Errs make.out

spell:	${INTRO} ${TUTOR} ${PAPER} ${APNDX1} ${APNDX2} ${APNDX3}
	@for i in ${INTRO} ${TUTOR} ${PAPER} ${APNDX1} ${APNDX2} ${APNDX3}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
