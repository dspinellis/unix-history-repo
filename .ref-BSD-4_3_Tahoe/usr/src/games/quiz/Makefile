#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.5	(Berkeley)	9/22/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	quiz.c
OBJS=

all: quiz

quiz: ${LIBC}
	${CC} -o $@ ${CFLAGS} ${SRCS}

clean: FRC
	rm -f ${OBJS} core quiz

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	-[ -d ${DESTDIR}/usr/games/lib/quiz.k ] || mkdir ${DESTDIR}/usr/games/lib/quiz.k
	chmod 700 ${DESTDIR}/usr/games/lib/quiz.k
	chown games.bin ${DESTDIR}/usr/games/lib/quiz.k
	cd quiz.k; install -c -o games -g bin -m 400 * ${DESTDIR}/usr/games/lib/quiz.k
	install -s -o games -g bin -m 700 quiz ${DESTDIR}/usr/games/hide
	(cd ${DESTDIR}/usr/games; rm -f quiz; ln -s dm quiz; chown games.bin quiz)

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

quiz: quiz.c /usr/include/stdio.h /usr/include/signal.h
quiz: /usr/include/machine/trap.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
