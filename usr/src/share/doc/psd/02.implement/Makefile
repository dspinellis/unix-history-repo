#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.2 (Berkeley) %G%
#
SRCS=	implement
PICFILES= fig1.pic fig2.pic
MACROS=	-ms
PRINTER=Pdp
TROFF=	ditroff -${PRINTER}
REFER=	refer -p /usr/old/dict/papers/Ind -e
PIC= pic -${PRINTER}

paper:	paper.${PRINTER}
	lpr -${PRINTER} -n paper.${PRINTER}

paper.${PRINTER}: ${SRCS} ${PICFILES}
	soelim   ${SRCS} > paper.pic
	${REFER} paper.pic | ${PIC} | ${TROFF} ${MACROS} -t > paper.${PRINTER}

clean:
	rm -f paper.[PT]* *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
