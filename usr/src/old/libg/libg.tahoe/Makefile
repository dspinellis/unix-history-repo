#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	dbxxx.c
OBJS=	dbxxx.o

all: libg.a

libg.a: dbxxx.c
	${CC} -E dbxxx.c | ${AS} -o libg.a

clean: FRC
	rm -f ${OBJS} core libg.a

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -o bin -g bin -m 644 libg.a ${DESTDIR}/usr/lib/libg.a

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

dbxxx.o: dbxxx.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
