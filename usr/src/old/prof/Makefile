#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	%G%
#
# If you don't want to plot, take out the -Dplot and the loader
# reference to plot.a
CFLAGS=	-O -Dplot
LIBC=	/lib/libc.a
SRCS=	prof.c
OBJS=	prof.o

all: prof

prof: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS} -lplot

clean: FRC
	rm -f ${OBJS} core prof

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 prof ${DESTDIR}/usr/bin/prof

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

prof.o: prof.c /usr/include/stdio.h /usr/include/sys/types.h
prof.o: /usr/include/sys/stat.h /usr/include/a.out.h /usr/include/sys/exec.h
prof.o: /usr/include/sys/time.h /usr/include/time.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
