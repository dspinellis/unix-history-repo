#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.1 (Berkeley) %G%
#
SRCS=	tutor.me
GSRCS=	gremlins
MACROS=	-me
PRINTER=Pdp
SOELIM=	soelim
GRIND=	vgrind -f
TBL=	dtbl -${PRINTER}
TROFF=	ditroff -${PRINTER}

paper:	paper.${PRINTER} pics.Pip
	lpr -${PRINTER} -n paper.${PRINTER}
	lpr -Pip -n pics.Pip

paper.${PRINTER}: ${SRCS}
	${SOELIM} ${SRCS} | ${GRIND} | ${TBL} | \
	${TROFF} ${MACROS} -t > paper.${PRINTER}

pics.Pip: ${GSRCS}
	grn -Pip ${GSRCS} | ditroff -Pip -t >pics.Pip

clean:
	rm -f paper.[PT]* *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
