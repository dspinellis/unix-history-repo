#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.6	(Berkeley)	7/30/87
#
DESTDIR=
CFLAGS=-O
SRCS=	compact.c tree.c uncompact.c
OBJS=	compact.o tree.o uncompact.o
MDIR=	/usr/man/cat.old
LINKS=	

.SUFFIXES: .1 .0

.1.0:
	/usr/man/manroff $*.1 > $*.0

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

depend:
	mkdep ${SRCS}

tags:
	ctags ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

compact.o: compact.c /usr/include/strings.h compact.h /usr/include/sys/param.h
compact.o: /usr/include/sys/types.h /usr/include/signal.h
compact.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
compact.o: /usr/include/machine/endian.h /usr/include/sys/stat.h
compact.o: /usr/include/sys/dir.h /usr/include/stdio.h
tree.o: tree.c compact.h /usr/include/sys/param.h /usr/include/sys/types.h
tree.o: /usr/include/signal.h /usr/include/machine/trap.h
tree.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
tree.o: /usr/include/sys/stat.h /usr/include/sys/dir.h /usr/include/stdio.h
uncompact.o: uncompact.c compact.h /usr/include/sys/param.h
uncompact.o: /usr/include/sys/types.h /usr/include/signal.h
uncompact.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
uncompact.o: /usr/include/machine/endian.h /usr/include/sys/stat.h
uncompact.o: /usr/include/sys/dir.h /usr/include/stdio.h /usr/include/strings.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
