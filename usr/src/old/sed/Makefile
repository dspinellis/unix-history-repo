#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	%G%
#
CFLAGS=	-O -w
LIBC=	/lib/libc.a
SRCS=	sed0.c sed1.c
OBJS=	sed0.o sed1.o

all: sed

sed:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core sed

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 sed ${DESTDIR}/bin/sed

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

sed0.o: sed0.c /usr/include/stdio.h sed.h
sed1.o: sed1.c /usr/include/stdio.h sed.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
