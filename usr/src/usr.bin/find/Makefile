#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.4	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	find.c bigram.c code.c
OBJS=	find.o bigram.o code.o

all: find bigram code

find: find.c ${LIBC}
	${CC} ${CFLAGS} -o $@ find.c

bigram: bigram.c
	${CC} ${CFLAGS} -o $@ bigram.c

code: code.c
	${CC} ${CFLAGS} -o $@ code.c

clean: FRC
	rm -f ${OBJS} core find bigram code

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 find ${DESTDIR}/usr/bin/find
	-mkdir ${DESTDIR}/usr/lib/find > /dev/null 2>&1
	install -s -o bin -g bin -m 755 bigram ${DESTDIR}/usr/lib/find/bigram
	install -s -o bin -g bin -m 755 code ${DESTDIR}/usr/lib/find/code
	install -c -o bin -g bin -m 755 updatedb.csh ${DESTDIR}/usr/lib/find/updatedb

lint: FRC
	lint ${CFLAGS} find.c
	lint ${CFLAGS} bigram.c
	lint ${CFLAGS} code.c

tags: FRC
	ctags find.c
	ctags -a bigram.c
	ctags -a code.c
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

find: find.c /usr/include/stdio.h /usr/include/sys/param.h
find: /usr/include/sys/types.h /usr/include/signal.h
find: /usr/include/machine/machparam.h /usr/include/sys/dir.h
find: /usr/include/sys/stat.h /usr/include/pwd.h /usr/include/grp.h
find: /usr/include/utmp.h
bigram: bigram.c /usr/include/stdio.h
code: code.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
