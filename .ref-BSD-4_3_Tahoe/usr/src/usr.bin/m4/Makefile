#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.3	(Berkeley)	6/18/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	m4.c m4y.c
OBJS=	m4.o m4y.o

all: m4

m4: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core m4 m4y.c

depend: ${SRCS} FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 m4 ${DESTDIR}/usr/bin/m4

lint: ${SRCS} FRC
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS} FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

m4.o: m4.c /usr/include/stdio.h /usr/include/signal.h
m4.o: /usr/include/machine/trap.h
m4y.o: m4y.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
