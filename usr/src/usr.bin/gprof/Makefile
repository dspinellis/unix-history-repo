SCCSID = @(#)Makefile	1.18 (Berkeley) %G%

DFLAGS = 
CFLAGS = -O ${DFLAGS}
LINTFLAGS = -x -a -n ${DFLAGS}

RM=	/bin/rm -f
INSTALL=cp

GPROFHDRS =	gprof.h
GPROFSRCS =	gprof.c arcs.c dfn.c lookup.c calls.c hertz.c \
		printgprof.c printlist.c
GPROFOBJS =	gprof.o arcs.o dfn.o lookup.o calls.o hertz.o \
		printgprof.o printlist.o

gprof: ${GPROFOBJS}
	cc -o gprof ${CFLAGS} ${GPROFOBJS}

install: gprof gprof.flat.blurb gprof.callg.blurb
	${INSTALL} gprof ${DESTDIR}/usr/ucb/gprof
	cp gprof.flat.blurb ${DESTDIR}/usr/lib
	cp gprof.callg.blurb ${DESTDIR}/usr/lib

clean:
	${RM} ${GPROFOBJS} errs gprof

gcrt0.h:
	cp /usr/src/libc/csu/gcrt0.h gcrt0.h

print:
	@ ls -l | pr
	@ pr makefile
	@ pr gcrt0.h
	@ pr ${GPROFHDRS} ${GPROFSRCS}
	@ pr gprof.flat.blurb gprof.callg.blurb

lint:
	lint ${LINTFLAGS} ${DFLAGS} ${GPROFSRCS}

gprof.o: gprof.c gprof.h gcrt0.h
arcs.o: arcs.c gprof.h gcrt0.h
lookup.o: lookup.c gprof.h gcrt0.h
dfn.o: dfn.c gprof.h gcrt0.h
calls.o: calls.c gprof.h gcrt0.h
hertz.o: gprof.h hertz.c
printgprof.o: printgprof.c gprof.h gcrt0.h
printlist.o: printlist.c gprof.h gcrt0.h
