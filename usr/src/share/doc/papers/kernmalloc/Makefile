#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.2 (Berkeley) %G%
#
SRCS=	kernmalloc.t appendix.t
FIGS=	alloc.fig usage.tbl
MACROS=	-ms
PRINTER=lz
SOELIM=	soelim
TBL=	dtbl -P${PRINTER}
EQN=	deqn -P${PRINTER}
PIC=	pic -P${PRINTER}
GRIND=	vgrind -f
TROFF=	rsh vangogh ditroff -P${PRINTER} ${MACROS}

print: paper.${PRINTER}
	lpr -P${PRINTER} -n paper.${PRINTER}

paper.${PRINTER}: ${SRCS} ${FIGS}
	${SOELIM} ${SRCS} | ${TBL} | ${PIC} | ${EQN} | ${GRIND} | \
		${TROFF} -t - >paper.${PRINTER}

clean:
	rm -f paper.${PRINTER} paper.[PT]* *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
