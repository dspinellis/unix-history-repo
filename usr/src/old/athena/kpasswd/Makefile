#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
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

CDEFS=	-DKERBEROS
CFLAGS=	-O
LIBC=	/lib/libc.a
KLIB=	/usr/local/lib/libkrb.a
DESLIB=	/usr/local/lib/libdes.a
KDBLIB=	/usr/local/lib/libkrb_dbm.a
INCDIR=	/usr/include/kerberos
SRC1=	kpasswd.c
SRC2=	kpasswdd.c
OBJS=	kpasswd.o kpasswdd.o
MAN=	kpasswd.0 kpasswdd.0

all: kpasswd kpasswdd

kpasswd: ${SRC1} ${LIBC} ${KLIB} ${DESLIB} kpasswd.c
	${CC} -o $@ -I${INCDIR} ${CFLAGS} ${CDEFS} ${SRC1} ../lib/des_rw.o -lutil -lkrb -ldes

kpasswdd: ${SRC2} ${LIBC} ${KLIB} ${DESLIB} kpasswdd.c
	${CC} -o $@ -I${INCDIR} ${CFLAGS} ${CDEFS} ${SRC2} ../lib/des_rw.o -lutil -lkrb -lkrb_dbm -ldes

clean:
	rm -f ${OBJS} core kpasswd kpasswdd

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRC1} ${SRC2}
	mkdep -p ${CFLAGS} ${SRC1} ${SRC2}

install: ${MAN}
	install -s -o bin -g bin -m 755 kpasswdd ${DESTDIR}/etc/kpasswdd
	install -s -o bin -g bin -m 755 kpasswd ${DESTDIR}/usr/local
	install -c -o bin -g bin -m 444 kpasswd.0 ${DESTDIR}/usr/man/cat1
	install -c -o bin -g bin -m 444 kpasswdd.0 ${DESTDIR}/usr/man/cat8

lint: ${SRC1} ${SRC2}
	lint ${CFLAGS} ${SRC1} ${SRC2}

tags: ${SRC1} ${SRC2}
	ctags ${SRC1} ${SRC2}
