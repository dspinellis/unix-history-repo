#
# Copyright (c) 1988 Regents of the University of California.
# All rights reregisterded.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation, advertising
# materials, and other materials related to such redistribution and
# use acknowledge that the software was developed by the University
# of California, Berkeley.  The name of the University may not be
# used to endorse or promote products derived from this software
# without specific prior written permission.  THIS SOFTWARE IS PROVIDED
# ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
# WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND
# FITNESS FOR A PARTICULAR PURPOSE.
#
# @(#)Makefile	5.3 (Berkeley) 9/20/88
#

CFLAGS=	-O -DKERBEROS
LIBC=	/lib/libc.a
SRC1=	registerd.c
OBJ1=	registerd.o
SRC2=	register.c
OBJ2=	register.o
SRC3=	make_keypair.c
OBJ3=	make_keypair.o
OBJS=	register.o registerd.o make_keypair.o
MAN=	register.0 registerd.0 make_keypair.0

all: registerd register make_keypair

registerd: ${OBJ1} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJ1} -lutil -lkdb -lkrb -ldes -lkrb

register: ${OBJ2} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJ2} -lutil -lkrb -ldes -lkrb

make_keypair: ${OBJ3} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJ3} -lutil -lkdb -lkrb -ldes

clean:
	rm -f ${OBJS} core registerd register make_keypair

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRC1} ${SRC2} ${SRC3}
	mkdep -p ${CFLAGS} ${SRC1} ${SRC2} ${SRC3}

#install: ${MAN}
install:
	install -s -o bin -g bin -m 755 registerd ${DESTDIR}/usr/libexec
	install -s -o root -g bin -m 4755 register ${DESTDIR}/usr/athena/register
	install -c -o bin -g bin -m 444 registerd.0 ${DESTDIR}/usr/man/cat8
	install -c -o bin -g bin -m 444 register.0 ${DESTDIR}/usr/man/cat1
	install -c -o bin -g bin -m 444 make_keypair.0 ${DESTDIR}/usr/man/cat8

lint: ${SRC1} ${SRC2} ${SRC3}
	lint ${CFLAGS} ${SRC1} ${SRC2} ${SRC3}

tags: ${SRC1} ${SRC2} ${SRC3}
	ctags ${SRC1} ${SRC2} ${SRC3}
