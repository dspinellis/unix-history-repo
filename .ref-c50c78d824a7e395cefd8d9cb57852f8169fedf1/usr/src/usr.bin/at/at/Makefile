#
# Copyright (c) 1988 The Regents of the University of California.
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
#	@(#)Makefile	5.7 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	atq.c atrm.c at.c atrun.c
OBJS=	atq.o atrm.o at.o atrun.o
ATRUNDIR=	/usr/lib
SPOOLDIRS=	/usr/spool/at /usr/spool/at/past
SPOOLFILES=	/usr/spool/at/lasttimedone
MAN=	at.0 atq.0 atrm.0

all: atq atrm at atrun

at atq atrm atrun: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

clean:
	rm -f ${OBJS} core atq atrm at atrun

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	-for i in ${SPOOLDIRS}; do \
		mkdir ${DESTDIR}/$$i > /dev/null 2>&1; \
		chown root ${DESTDIR}/$$i; \
		chgrp daemon ${DESTDIR}/$$i; \
		chmod 755 ${DESTDIR}/$$i; \
	done
	-for i in ${SPOOLFILES}; do \
		if [ ! -f ${DESTDIR}/$$i ]; then \
			echo 0 > ${DESTDIR}/$$i; \
		fi; \
		chown root ${DESTDIR}/$$i; \
		chgrp daemon ${DESTDIR}/$$i; \
	done
	install -s -o bin -g bin -m 755 atrun ${DESTDIR}/${ATRUNDIR}
	install -s -o root -g bin -m 4755 atq ${DESTDIR}/usr/bin
	install -s -o root -g bin -m 4755 at ${DESTDIR}/usr/bin
	install -s -o root -g bin -m 4755 atrm ${DESTDIR}/usr/bin
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} atq.c
	lint ${CFLAGS} atrm.c
	lint ${CFLAGS} at.c
	lint ${CFLAGS} atrun.c

tags: ${SRCS}
	ctags atq.c
	ctags -a atrm.c
	ctags -a at.c
	ctags -a atrun.c
	sort -o tags tags
