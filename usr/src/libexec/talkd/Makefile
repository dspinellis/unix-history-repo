#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
DESTDIR=
OBJS=	talkd.o announce.o process.o table.o print.o
SRCS=	talkd.c announce.c process.c table.c print.c
CFLAGS= -O

all:	talkd

talkd: ${OBJS}
	cc ${CFLAGS} -o talkd ${OBJS}

install: talkd
	install -s talkd ${DESTDIR}/etc/talkd

clean:
	rm -f ${OBJS} errs core a.out talkd

tags:	${SRCS}
	ctags ${SOURCE}
