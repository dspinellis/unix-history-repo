#
# Copyright (c) 1989 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.2 (Berkeley) %G%
#
V1SRCS=	A.t B.t C.t D.t E.t F.t G.t H.t I.t J.t
V2SRCS=	V2A.t V2B.t
MACROS=	-ms
PRINTER=Plz
TROFF=	ditroff -${PRINTER}

all: V1.${PRINTER} V2.${PRINTER}

V1.${PRINTER}: ${V1SRCS}
	${TROFF} ${MACROS} -t ${V1SRCS} | psdit >V1.${PRINTER}

V2.${PRINTER}: ${V2SRCS}
	${TROFF} ${MACROS} -t ${V2SRCS} | psdit >V2.${PRINTER}

print: V1.${PRINTER} V2.${PRINTER}
	lpr -${PRINTER} V1.${PRINTER} V2.${PRINTER}

clean:
	rm -f *.spell errs Errs make.out V1.${PRINTER} V2.${PRINTER}

spell:	${V1SRCS} ${V2SRCS}
	@for i in ${V1SRCS} ${V2SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
