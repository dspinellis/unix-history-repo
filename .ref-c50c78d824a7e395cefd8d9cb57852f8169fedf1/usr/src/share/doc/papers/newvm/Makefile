#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.1 (Berkeley) %G%
#
SRCS=	0.t 1.t a.t
MACROS=	-ms
PRINTER=Plz
TBL=	dtbl -${PRINTER}
TROFF=	ptroff -${PRINTER}

paper: ${SRCS}
	${TBL} ${SRCS} | ${TROFF} ${MACROS}

clean:
	rm -f paper.[PT]* *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
