#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.1 (Berkeley) %G%
#
SRCS=	0.t 1.t 2.t r.t
MACROS=	-ms
PRINTER=Plz
TROFF=	ptroff -${PRINTER}

paper: ${SRCS}
	${TROFF} ${MACROS} ${SRCS}

clean:
	rm -f paper.[PT]* *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
