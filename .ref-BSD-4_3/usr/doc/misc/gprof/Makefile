#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) 6/17/86
#
SRCS=	header.me abstract.me intro.me profiling.me gathering.me \
	postp.me present.me refs.me
PICSRC= postp1.pic postp2.pic postp3.pic pres1.pic pres2.pic
MACROS=	-me
PRINTER=Pdp
SOELIM=	soelim
EQN=	deqn -${PRINTER}
TBL=	dtbl -${PRINTER}
PIC= 	pic -${PRINTER}
TROFF=	ditroff -${PRINTER}

paper:	paper.${PRINTER}
	lpr -${PRINTER} -n paper.${PRINTER}

paper.${PRINTER}: ${SRCS} ${PICSRC}
	${SOELIM} ${SRCS} | ${PIC} | ${TBL} | ${EQN} | \
	${TROFF} ${MACROS} -t > paper.${PRINTER}

clean:
	rm -f paper.[PTp]* *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
