#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
SCCSID = @(#)Makefile	5.1 (Berkeley) %G%

DFLAGS = 
CFLAGS = -O ${DFLAGS}
LINTFLAGS = -x -a -n ${DFLAGS}

RM=	/bin/rm -f
INSTALL=install
PR=pr

GPROFHDRS =	gprof.h vax.h
GPROFSRCS =	gprof.c arcs.c dfn.c lookup.c calls.c hertz.c \
		printgprof.c printlist.c
GPROFOBJS =	gprof.o arcs.o dfn.o lookup.o calls.o hertz.o \
		printgprof.o printlist.o

gprof: ${GPROFOBJS}
	cc -o gprof ${CFLAGS} ${GPROFOBJS}

install: gprof gprof.flat gprof.callg
	install -s gprof ${DESTDIR}/usr/ucb/gprof
	install -c -m 644 gprof.flat ${DESTDIR}/usr/lib
	install -c -m 644 gprof.callg ${DESTDIR}/usr/lib

clean:
	${RM} ${GPROFOBJS} errs gprof

gcrt0.h:
	cp /usr/src/libc/csu/gcrt0.h gcrt0.h

print:
	@ ls -l | ${PR}
	@ ${PR} makefile
	@ ${PR} gcrt0.h
	@ ${PR} ${GPROFHDRS} ${GPROFSRCS}
	@ ${PR} gprof.flat gprof.callg

lint:
	lint ${LINTFLAGS} ${DFLAGS} ${GPROFSRCS}

gprof.o: gprof.c gprof.h vax.h gcrt0.h
arcs.o: arcs.c gprof.h vax.h gcrt0.h
lookup.o: lookup.c gprof.h vax.h gcrt0.h
dfn.o: dfn.c gprof.h vax.h gcrt0.h
calls.o: calls.c gprof.h vax.h gcrt0.h
hertz.o: gprof.h vax.h hertz.c
printgprof.o: printgprof.c gprof.h vax.h gcrt0.h
printlist.o: printlist.c gprof.h vax.h gcrt0.h
