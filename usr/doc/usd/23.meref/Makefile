#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	6.2 (Berkeley) 4/16/86
#
SRCS=	ref.me
DEVICE=	dp
TROFF=	ditroff -P${DEVICE}

paper:	ref.${DEVICE}
	lpr -P${DEVICE} -n ref.${DEVICE}

ref.${DEVICE}: ${SRCS}
	${TROFF} -me -t ${SRCS} > ref.${DEVICE}

clean:
	rm -f ref.${DEVICE} *.spell errs Errs make.out

spell:	${SRCS}
	@for i in ${SRCS}; do \
		echo $$i; spell $$i | sort | comm -23 - spell.ok > $$i.spell; \
	done
