#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	tp0.c tp1.c tp2.c tp3.c
OBJS=	tp0.o tp1.o tp2.o tp3.o

all: tp

tp:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core tp

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 tp ${DESTDIR}/bin/tp

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

tp0.o: tp0.c tp.h tp_defs.h
tp1.o: tp1.c tp.h
tp2.o: tp2.c tp.h /usr/include/stdio.h /usr/include/sys/param.h
tp2.o: /usr/include/sys/types.h /usr/include/signal.h
tp2.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
tp2.o: /usr/include/sys/dir.h
tp3.o: tp3.c tp.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
