#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.1 (Berkeley) 5/7/86
#
SRCS=	rcs.ms
MSRC=	rcs.man
MACROS=	-msU
PRINTER=Pdp
TROFF=	ditroff -${PRINTER}

paper:	paper.${PRINTER} manpg.${PRINTER}
	lpr -${PRINTER} -n paper.${PRINTER}
	lpr -${PRINTER} -n manpg.${PRINTER}

paper.${PRINTER}: ${SRCS}
	${TROFF} ${MACROS} -t ${SRCS} > paper.${PRINTER}

manpg.${PRINTER}: ${MSRC}
	${TROFF} -man -t ${MSRC} > manpg.${PRINTER}

clean:
	rm -f manpg.[PT]* paper.[PT]* *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
