#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1	(Berkeley)	12/12/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	fsplit.c
OBJS=	fsplit.o

all: fsplit

fsplit:	${SRCS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${SRCS

clean: FRC
	rm -f ${OBJS} core fsplit

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 fsplit ${DESTDIR}/usr/ucb/fsplit

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

fsplit: fsplit.c /usr/include/ctype.h /usr/include/stdio.h
fsplit: /usr/include/sys/types.h /usr/include/sys/stat.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
