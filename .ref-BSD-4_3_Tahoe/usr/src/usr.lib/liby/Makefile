#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2	(Berkeley)	6/30/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	libmai.c libzer.c
OBJS=	libmai.o libzer.o

all: liby.a

liby.a: ${OBJS}
	ar cr $@ ${OBJS}

clean: FRC
	rm -f ${OBJS} core liby.a

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -o bin -g bin -m 644 liby.a ${DESTDIR}/usr/lib/liby.a
	ranlib ${DESTDIR}/usr/lib/liby.a

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

libmai.o: libmai.c
libzer.o: libzer.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
