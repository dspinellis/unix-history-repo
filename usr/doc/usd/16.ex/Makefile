#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.1 (Berkeley) 5/30/86
#
SRCS=	ex.rm
MACROS=	-msU
PRINTER=Pdp
TBL=	dtbl -${PRINTER}
TROFF=	ditroff -${PRINTER}

paper:	paper.${PRINTER} summary.${PRINTER}
	lpr -${PRINTER} -n paper.${PRINTER}
	lpr -${PRINTER} -n summary.${PRINTER}

paper.${PRINTER}: ${SRCS}
	${TROFF} ${MACROS} -t ${SRCS} > paper.${PRINTER}

summary.${PRINTER}: ex.summary
	${TBL} ex.summary | ${TROFF} ${MACROS} -t > summary.${PRINTER}

clean:
	rm -f paper.[PT]* summary.[PT]* *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
