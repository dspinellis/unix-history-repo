#
# Copyright (c) 1987 The Regents of the University of California.
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
#	@(#)Makefile	4.7 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	quiz.c
OBJS=
MAN=	quiz.0
CATS=	africa america areas arith asia babies bard chinese collectives \
	ed elements europe greek inca index latin locomotive midearth morse \
	murders poetry posneg pres province seq-easy seq-hard sexes sov \
	spell state trek ucc

all: quiz

quiz: ${LIBC}
	${CC} -o $@ ${CFLAGS} $@.c

clean:
	rm -f ${OBJS} core quiz

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
	-[ -d ${DESTDIR}/usr/games/lib/quiz.k ] || mkdir ${DESTDIR}/usr/games/lib/quiz.k
	chmod 700 ${DESTDIR}/usr/games/lib/quiz.k
	chown games.bin ${DESTDIR}/usr/games/lib/quiz.k
	cd quiz.k; install -c -o games -g bin -m 400 ${CATS} ${DESTDIR}/usr/games/lib/quiz.k
	install -s -o games -g bin -m 700 quiz ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f quiz; ln -s dm quiz; chown games.bin quiz)
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat6

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
