#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific written prior permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.10 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
HDRS=	gprof.h ${MACHINE}.h
SRCS=	gprof.c arcs.c dfn.c lookup.c ${MACHINE}.c hertz.c \
	printgprof.c printlist.c
OBJS=	gprof.o arcs.o dfn.o lookup.o ${MACHINE}.o hertz.o \
	printgprof.o printlist.o

all: gprof

gprof: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

gcrt0.h: FRC
	-if [ -r gcrt0.h ] && \
		cmp -s gcrt0.h ../../lib/libc/${MACHINE}/csu/gmon.h; then \
		:; \
	else \
		rm -f gcrt0.h; \
		cp ../../lib/libc/${MACHINE}/csu/gmon.h gcrt0.h; \
	fi

clean: FRC
	rm -f ${OBJS} core gprof

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 gprof ${DESTDIR}/usr/ucb/gprof
	install -c -o bin -g bin -m 644 gprof.flat ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 644 gprof.callg ${DESTDIR}/usr/lib

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

gprof.o: gprof.c gprof.h /usr/include/stdio.h /usr/include/sys/types.h
gprof.o: /usr/include/sys/stat.h /usr/include/a.out.h /usr/include/sys/exec.h
gprof.o: gcrt0.h tahoe.h
arcs.o: arcs.c gprof.h /usr/include/stdio.h /usr/include/sys/types.h
arcs.o: /usr/include/sys/stat.h /usr/include/a.out.h /usr/include/sys/exec.h
arcs.o: gcrt0.h tahoe.h
dfn.o: dfn.c /usr/include/stdio.h gprof.h /usr/include/stdio.h
dfn.o: /usr/include/sys/types.h /usr/include/sys/stat.h /usr/include/a.out.h
dfn.o: /usr/include/sys/exec.h gcrt0.h tahoe.h
lookup.o: lookup.c gprof.h /usr/include/stdio.h /usr/include/sys/types.h
lookup.o: /usr/include/sys/stat.h /usr/include/a.out.h /usr/include/sys/exec.h
lookup.o: gcrt0.h tahoe.h
tahoe.o: tahoe.c gprof.h /usr/include/stdio.h /usr/include/sys/types.h
tahoe.o: /usr/include/sys/stat.h /usr/include/a.out.h /usr/include/sys/exec.h
tahoe.o: gcrt0.h tahoe.h
hertz.o: hertz.c /usr/include/sys/time.h /usr/include/time.h
printgprof.o: printgprof.c gprof.h /usr/include/stdio.h
printgprof.o: /usr/include/sys/types.h /usr/include/sys/stat.h
printgprof.o: /usr/include/a.out.h /usr/include/sys/exec.h gcrt0.h tahoe.h
printlist.o: printlist.c gprof.h /usr/include/stdio.h /usr/include/sys/types.h
printlist.o: /usr/include/sys/stat.h /usr/include/a.out.h
printlist.o: /usr/include/sys/exec.h gcrt0.h tahoe.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
