SCCSID = @(#)Makefile	1.10 (Berkeley) %G%

DFLAGS = 
CFLAGS = -O ${DFLAGS}
LINTFLAGS = -x -a -n

INSTALL=cp

GPROFHDRS =	gprof.h
GPROFSRCS =	gprof.c arcs.c dfn.c lookup.c printgprof.c calls.c
GPROFOBJS =	gprof.o arcs.o dfn.o lookup.o printgprof.o calls.o

gprof: ${GPROFOBJS}
	cc -o gprof ${CFLAGS} ${GPROFOBJS}

install: gprof flat.blurb callg.blurb
	${INSTALL} gprof ${DESTDIR}/usr/bin/gprof
	cp flat.blurb ${DESTDIR}/usr/lib/flat.blurb
	cp callg.blurb ${DESTDIR}/usr/lib/callg.blurb

lint:
	lint ${CFLAGS} ${LINTFLAGS} ${GPROFSRCS}

gprof.o: gprof.c gprof.h gcrt0.h
arcs.o: arcs.c gprof.h gcrt0.h
lookup.o: lookup.c gprof.h gcrt0.h
dfn.o: dfn.c gprof.h gcrt0.h
printgprof.o: printgprof.c gprof.h gcrt0.h
calls.o: calls.c gprof.h gcrt0.h
