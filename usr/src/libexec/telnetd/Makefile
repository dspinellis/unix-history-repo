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

DEFINES= -DNEED_GETTOS
LIBC=	/lib/libc.a
# 4.4BSD
#		LIBS= -lutil -ltermcap
#		GETTYOBJ= get_date.o gettytab.o
#		GETTYSRC= get_date.c gettytab.c
#		LIBEXEC=${DESTDIR}/usr/libexec
#		VPATH=../getty
#			or
#		VPATH=/usr/src/libexec/getty
#
# 4.3BSD	LIBS= -ltermcap
#		GETTYOBJ= get_date.o gettytab.o
#		GETTYSRC= get_date.c gettytab.c
#		LIBEXEC=${DESTDIR}/etc
#		VPATH=../getty
#			or
#		VPATH=/usr/src/etc/getty
#
# Cray UNICOS
#		LIBS= -lnet -lcurses
#		GETTYOBJ=
#		GETTYSRC=
#		LIBEXEC=${DESTDIR}/etc

LIBS=	-lutil -ltermcap
GETTYOBJ= get_date.o gettytab.o
GETTYSRC= get_date.c gettytab.c
LIBEXEC=${DESTDIR}/usr/libexec
VPATH=	../getty

SRC1=	telnetd.c state.c termstat.c slc.c sys_term.c utility.c global.c
OBJ1=	telnetd.o state.o termstat.o slc.o sys_term.o utility.o global.o

OBJS=	${OBJ1} ${GETTYOBJ}
SRCS=	${SRC1} ${GETTYSRC}

MAN=	telnetd.0
CFLAGS=	-O ${DEFINES}

#
# These next three lines are not needed in 4.4BSD
#
#.SUFFIXES: .0 .8
#.8.0:
#	nroff -man -h $< > $@


all: telnetd

telnetd: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} ${LIBS}

clean:
	rm -f ${OBJS} core telnetd

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 telnetd ${LIBEXEC}
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat8

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
