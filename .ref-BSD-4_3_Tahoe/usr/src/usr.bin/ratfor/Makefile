#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.3	(Berkeley)	6/18/87
#
CFLAGS=	-O
YFLAGS=	-d
LIBC=	/lib/libc.a
SRCS=	r.c r0.c r1.c r2.c rio.c rlook.c rlex.c
OBJS=	r.o r0.o r1.o r2.o rio.o rlook.o rlex.o

all: ratfor

ratfor:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core ratfor r.c

depend: ${SRCS} FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 ratfor ${DESTDIR}/usr/bin/ratfor

lint: ${SRCS} FRC
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS} FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

r.o: r.c
r0.o: r0.c r.h /usr/include/stdio.h y.tab.h
r1.o: r1.c r.h /usr/include/stdio.h y.tab.h
r2.o: r2.c r.h /usr/include/stdio.h y.tab.h
rio.o: rio.c r.h /usr/include/stdio.h y.tab.h
rlook.o: rlook.c
rlex.o: rlex.c r.h /usr/include/stdio.h y.tab.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
