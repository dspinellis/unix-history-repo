#
# Copyright (c) 1989 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.1 (Berkeley) %G%
#
SRCS=	A.t B.t C.t D.t E.t F.t G.t H.t I.t J.t
MACROS=	-ms
PRINTER=Plz
TROFF=	ditroff -${PRINTER}

paper.${PRINTER}: ${SRCS}
	${TROFF} ${MACROS} -t ${SRCS} | psdit >paper.${PRINTER}

print: paper.${PRINTER}
	lpr -${PRINTER} paper.${PRINTER}

clean:
	rm -f *.spell errs Errs make.out paper.${PRINTER}

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
