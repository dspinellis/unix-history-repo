#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.1 (Berkeley) %G%
#
SRCS=	kernmalloc.ms
MACROS=	-ms
PRINTER=Plz
EQN=	deqn -${PRINTER}
TROFF=	ditroff -${PRINTER}

paper: ${SRCS}
	${EQN} ${SRCS} | ${TROFF} ${MACROS}

clean:
	rm -f paper.[PT]* *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
