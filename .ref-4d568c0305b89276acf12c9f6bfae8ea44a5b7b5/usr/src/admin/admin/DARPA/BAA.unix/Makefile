#
# Copyright (c) 1989 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.5 (Berkeley) %G%
#
V1SRCS=	cover.t A.t B.t C.t D.t E.t F.t G.t H.t I.t J.t
V2SRCS=	V2A.t V2B.t
MACROS=	-ms
TYPE=psc
PS=ps
PRINTER=lz
TBL=	dtbl -P${TYPE}
TROFF=	ditroff -T${TYPE}
NROFF=	nroff
NTBL=	tbl

all: V1.${PS} V2.${PS}

V1.${PS}: ${V1SRCS}
	${TROFF} ${PAGES} ${MACROS} -t ${V1SRCS} | psdit >$@

V2.${PS}: ${V2SRCS}
	${TBL} ${V2SRCS} | ${TROFF} ${PAGES} ${MACROS} -t | psdit >$@

print: V1.${PS} V2.${PS}
	lpr -P${PRINTER} V1.ps V2.ps

nroff.out: ${V1SRCS} ${V2SRCS}
	${NTBL} ${V1SRCS} ${V2SRCS} | ${NROFF} ${PAGES} ${MACROS} >$@

clean:
	rm -f *.spell errs Errs make.out V1.${PS} V2.${PS}

spell:	${V1SRCS} ${V2SRCS}
	@for i in ${V1SRCS} ${V2SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
