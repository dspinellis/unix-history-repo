#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.6 (Berkeley) %G%
#
DESTDIR=
DFLAGS= 
CFLAGS=	-O ${DFLAGS}
LINTFLAGS= -x -a -n ${DFLAGS}

RM=	/bin/rm -f
INSTALL=install
PR=pr
MACHINE=	unknown
GPROFHDRS=	gprof.h ${MACHINE}.h
GPROFSRCS=	gprof.c arcs.c dfn.c lookup.c ${MACHINE}.c hertz.c \
		printgprof.c printlist.c
GPROFOBJS=	gprof.o arcs.o dfn.o lookup.o ${MACHINE}.o hertz.o \
		printgprof.o printlist.o

gprof: ${GPROFOBJS}
	cc -o gprof ${CFLAGS} ${GPROFOBJS}

install: gprof gprof.flat gprof.callg
	install -s gprof ${DESTDIR}/usr/ucb/gprof
	install -c -m 644 gprof.flat ${DESTDIR}/usr/lib
	install -c -m 644 gprof.callg ${DESTDIR}/usr/lib

clean:
	${RM} ${GPROFOBJS} errs gprof

gcrt0.h: FRC
	-if [ -r gcrt0.h ] && \
	   cmp -s gcrt0.h /usr/src/lib/libc/${MACHINE}/csu/gmon.h; then \
	       :; \
	else \
	       rm -f gcrt0.h; \
	       cp /usr/src/lib/libc/${MACHINE}/csu/gmon.h gcrt0.h; \
	fi

FRC:

print:
	@ ls -l | ${PR}
	@ ${PR} makefile
	@ ${PR} gcrt0.h
	@ ${PR} ${GPROFHDRS} ${GPROFSRCS}
	@ ${PR} gprof.flat gprof.callg

lint:
	lint ${LINTFLAGS} ${DFLAGS} ${GPROFSRCS}

gprof.o: gprof.c gprof.h ${MACHINE}.h gcrt0.h
arcs.o: arcs.c gprof.h ${MACHINE}.h gcrt0.h
lookup.o: lookup.c gprof.h ${MACHINE}.h gcrt0.h
dfn.o: dfn.c gprof.h ${MACHINE}.h gcrt0.h
${MACHINE}.o: ${MACHINE}.c gprof.h ${MACHINE}.h gcrt0.h
hertz.o: gprof.h ${MACHINE}.h hertz.c
printgprof.o: printgprof.c gprof.h ${MACHINE}.h gcrt0.h
printlist.o: printlist.c gprof.h ${MACHINE}.h gcrt0.h
