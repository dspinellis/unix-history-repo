#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.2	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	fed.c subr.c io.c
OBJS=	fed.o subr.o io.o
LIBS=	-lm -l2648

all: fed

fed: ${OBJS} ${LIBC} /usr/lib/lib2648.a
	${CC} -o $@ ${CFLAGS} ${OBJS} ${LIBS}

clean: FRC
	rm -f ${OBJS} core fed

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 fed ${DESTDIR}/usr/old/fed

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

fed.o: fed.c fed.h /usr/include/stdio.h /usr/include/ctype.h
fed.o: /usr/include/vfont.h /usr/include/signal.h /usr/include/machine/trap.h
fed.o: /usr/include/setjmp.h
subr.o: subr.c fed.h /usr/include/stdio.h /usr/include/ctype.h
subr.o: /usr/include/vfont.h /usr/include/signal.h /usr/include/machine/trap.h
subr.o: /usr/include/setjmp.h
io.o: io.c fed.h /usr/include/stdio.h /usr/include/ctype.h /usr/include/vfont.h
io.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/setjmp.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
