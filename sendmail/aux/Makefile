#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.5 (Berkeley) 9/20/88
#
CFLAGS=	-O -I../src -I../include
ALL=	mconnect mailstats praliases mail rmail
SRCS=	mconnect.c mail.c mailstats.c praliases.c rmail.c
OBJS=	mconnect.o mail.o mailstats.o praliases.o rmail.o
MAN=	binmail.0 rmail.0

all: ${ALL}

rmail mconnect mailstats mail:
	${CC} ${CFLAGS} -o $@ $@.c

praliases:
	${CC} ${CFLAGS} -o $@ $@.c -ldbm

clean:
	rm -f core ${ALL} ${OBJS}

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o root -g bin -m 4755 mail ${DESTDIR}/bin/mail
	install -s -o bin -g bin -m 755 rmail ${DESTDIR}/bin/rmail
	install -s -o bin -g bin -m 755 mconnect mailstats praliases ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1
