#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.1 (Berkeley) 5/5/86
#
PRINTER=-Pdp
TROFF=	ditroff ${PRINTER}
TBL=	dtbl ${PRINTER}
LPR=	lpr -n ${PRINTER}

SRCS=	dbx.ms

paper:	dbx.n
	${LPR} dbx.n

dbx.n:	${SRCS}
	${TBL} ${SRCS} | ${TROFF} -t -msU >dbx.n

clean:
	rm -f dbx.n

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
