#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	6/18/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	graph.c
OBJS=	graph.o

all: graph

graph: ${SRCS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${SRCS} -lplot -lm

clean: FRC
	rm -f ${OBJS} core graph

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 graph ${DESTDIR}/usr/bin/graph

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

graph.o: graph.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/math.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
