#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.7	(Berkeley)	%G%
#
DESTDIR=
CFLAGS=-O
SRCS=	compact.c tree.c uncompact.c
OBJS=	compact.o tree.o uncompact.o
MDIR=	/usr/man/cat.old
LINKS=	

all: compact uncompact

compact: compact.o tree.o
	${CC} ${LDFLAGS} compact.o tree.o -o $@

uncompact: uncompact.o tree.o
	${CC} ${LDFLAGS} uncompact.o tree.o -o $@

install: compact.0
#	install -s -o bin -g bin -m 755 compact ${DESTDIR}/usr/old/compact
	install -s -o bin -g bin -m 755 uncompact ${DESTDIR}/usr/old/uncompact
	install -c -o bin -g bin -m 755 ccat.sh ${DESTDIR}/usr/old/ccat
	install -c -o bin -g bin -m 755 compact.0 ${DESTDIR}${MDIR}/compact.0
	(cd ${DESTDIR}${MDIR};rm -f ccat.0 uncompact.0)
	ln ${DESTDIR}${MDIR}/compact.0 ${DESTDIR}${MDIR}/ccat.0
	ln ${DESTDIR}${MDIR}/compact.0 ${DESTDIR}${MDIR}/uncompact.0

clean:
	rm -f compact uncompact ${OBJS}

cleandir: clean
	rm -f ${MAN} tags .depend

depend:
	mkdep ${SRCS}

tags:
	ctags ${SRCS}
