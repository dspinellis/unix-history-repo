#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.2 (Berkeley) 4/10/86
#
SRCS=	0.t 1.t 2.t 3.t 4.t 5.t 6.t 7.t 8.t a.t
DEVICE=	dp
TROFF=	ditroff -P${DEVICE}

paper:	uchanges
	lpr -P${DEVICE} -n uchanges

uchanges: ${SRCS}
	${TROFF} -ms -t ${SRCS} > uchanges

clean:
	rm -f uchanges ?.t.spell errs Errs

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
