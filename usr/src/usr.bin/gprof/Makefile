SCCSID = @(#)Makefile	1.16 (Berkeley) %G%

DFLAGS = 
CFLAGS = -O ${DFLAGS}
LINTFLAGS = -x -a -n ${DFLAGS}

RM=	/bin/rm -f
INSTALL=cp

GPROFHDRS =	gprof.h
GPROFSRCS =	gprof.c arcs.c dfn.c lookup.c printgprof.c calls.c printlist.c
GPROFOBJS =	gprof.o arcs.o dfn.o lookup.o printgprof.o calls.o printlist.o

gprof: ${GPROFOBJS}
	cc -o gprof ${CFLAGS} ${GPROFOBJS}

install: gprof flat.blurb callg.blurb
	${INSTALL} gprof ${DESTDIR}/usr/ucb/gprof
	cp flat.blurb ${DESTDIR}/usr/lib/flat.blurb
	cp callg.blurb ${DESTDIR}/usr/lib/callg.blurb

clean:
	${RM} ${GPROFOBJS} errs gprof

gcrt0.h:
	cp /usr/src/libc/csu/gcrt0.h gcrt0.h

print:
	@ ls -l | pr
	@ pr makefile
	@ pr gcrt0.h
	@ pr ${GPROFHDRS} ${GPROFSRCS}
	@ pr flat.blurb callg.blurb

lint:
	lint ${LINTFLAGS} ${DFLAGS} ${GPROFSRCS}

gprof.o: gprof.c gprof.h gcrt0.h
arcs.o: arcs.c gprof.h gcrt0.h
lookup.o: lookup.c gprof.h gcrt0.h
dfn.o: dfn.c gprof.h gcrt0.h
printgprof.o: printgprof.c gprof.h gcrt0.h
calls.o: calls.c gprof.h gcrt0.h
printlist.o: printlist.c gprof.h gcrt0.h
