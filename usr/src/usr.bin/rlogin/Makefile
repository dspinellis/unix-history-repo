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
# @(#)Makefile	5.2 (Berkeley) 5/11/89
#

CFLAGS=	-O -DKERBEROS -I/usr/include/kerberosIV
LIBC=	/lib/libc.a
SRCS=	rlogin.c kcmd/krcmd.c kcmd/kcmd.c des/des_rw.c
OBJS=	krcmd.o kcmd.o des_rw.o
MAN=	rlogin.0
VPATH=	des:kcmd

all: rlogin

rlogin: ${LIBC} ${OBJS}
	${CC} -o $@ ${CFLAGS} $@.c ${OBJS} -lkrb -ldes

clean:
	rm -f ${OBJS} core rlogin ${OBJS}

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o root -g bin -m 4755 rlogin ${DESTDIR}/usr/bin
	install -c -o bin -g bin -m 444 rlogin.0 ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
