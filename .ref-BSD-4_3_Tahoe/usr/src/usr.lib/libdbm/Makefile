#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4	(Berkeley)	6/19/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	dbm.c
OBJS=	dbm.o

all: libdbm.a libdbm_p.a

libdbm.a: dbm.c
	${CC} -c ${CFLAGS} dbm.c
	ld -r -o libdbm.a -x dbm.o
	rm -f dbm.o

libdbm_p.a: dbm.c
	${CC} -p -c ${CFLAGS} dbm.c
	ld -r -o libdbm_p.a -X dbm.o
	rm -f dbm.o

clean: FRC
	rm -f ${OBJS} core libdbm.a libdbm_p.a

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -o bin -g bin -m 644 libdbm.a ${DESTDIR}/usr/lib/libdbm.a
	install -o bin -g bin -m 644 libdbm_p.a ${DESTDIR}/usr/lib/libdbm_p.a
	install -c -o bin -g bin -m 444 dbm.h ${DESTDIR}/usr/include/dbm.h

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

dbm.o: dbm.c dbm.h /usr/include/ndbm.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
