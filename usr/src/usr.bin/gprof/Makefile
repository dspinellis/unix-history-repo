SCCSID = @(#)Makefile	1.9 (Berkeley) %G%

DFLAGS = 
CFLAGS = -O ${DFLAGS}
LINTFLAGS = -x -a -n

GPROFHDRS =	gprof.h
GPROFSRCS =	gprof.c arcs.c dfn.c lookup.c printgprof.c calls.c
GPROFOBJS =	gprof.o arcs.o dfn.o lookup.o printgprof.o calls.o
OTHERS =	gcrt0.c gcrt0.h gcrt0.ex

gprof: ${GPROFOBJS}
	cc -o gprof ${CFLAGS} ${GPROFOBJS}

gcrt0.o: gcrt0.s
	as -o gcrt0.o gcrt0.s

gcrt0.s: gcrt0.c gcrt0.h gcrt0.ex
	cc -S ${DFLAGS} gcrt0.c
	ex - gcrt0.s < gcrt0.ex
	/lib/cpp gcrt0.s > gcrt0.S
	mv gcrt0.S gcrt0.s

lint:
	lint ${CFLAGS} ${LINTFLAGS} ${GPROFSRCS}

${GPROFHDRS} ${GPROFSRCS} ${OTHERS}:
	sccs get $@

sources: ${GPROFHDRS} ${GPROFSRCS} ${OTHERS}

gprof.o: gprof.c gprof.h gcrt0.h
arcs.o: arcs.c gprof.h gcrt0.h
lookup.o: lookup.c gprof.h gcrt0.h
dfn.o: dfn.c gprof.h gcrt0.h
printgprof.o: printgprof.c gprof.h gcrt0.h
calls.o: calls.c gprof.h gcrt0.h
