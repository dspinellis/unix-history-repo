SCCSID = @(#)Makefile	1.5 (Berkeley) %G%

DFLAGS = 
CFLAGS = ${DFLAGS}

GPROFHDRS =	gprof.h calls.h
GPROFSRCS =	gprof.c arcs.c dfn.c lookup.c printgprof.c calls.c
GPROFOBJS =	gprof.o arcs.o dfn.o lookup.o printgprof.o calls.o
OTHERS =	gmcrt0.c gmcrt0.h gmcrt0.ex

gprof: ${GPROFOBJS}
	cc -o gprof ${CFLAGS} ${GPROFOBJS}

gmcrt0.o: gmcrt0.s
	as -o gmcrt0.o gmcrt0.s

gmcrt0.s: gmcrt0.c gmcrt0.h gmcrt0.ex
	cc -S ${DFLAGS} gmcrt0.c
	ex - gmcrt0.s < gmcrt0.ex
	/lib/cpp gmcrt0.s > gmcrt0.S
	mv gmcrt0.S gmcrt0.s

profit: profit.o gmcrt0.o
	/bin/ld -X gmcrt0.o -o profit profit.o -lcp

profit.o:
	cc -p -c profit.c

gprof.o: gprof.c gprof.h
arcs.o: arcs.c gprof.h
lookup.o: lookup.c gprof.h
dfn.o: dfn.c gprof.h
printgprof.o: printgprof.c gprof.h
calls.o: calls.c calls.h gprof.h

PGPROF = p.gprof.o p.arcs.o p.dfn.o p.lookup.o p.printgprof.o p.calls.o

p.gprof: ${PGPROF}
	/bin/ld -X gmcrt0.o -o p.gprof ${PGPROF} -lcp

p.gprof.o: gprof.c gprof.h
	cp gprof.c p.gprof.c
	cc -c -p ${CFLAGS} p.gprof.c
	rm p.gprof.c

p.arcs.o: arcs.c gprof.h
	cp arcs.c p.arcs.c
	cc -c -p ${CFLAGS} p.arcs.c
	rm p.arcs.c

p.dfn.o: dfn.c gprof.h
	cp dfn.c p.dfn.c
	cc -c -p ${CFLAGS} p.dfn.c
	rm p.dfn.c

p.lookup.o: lookup.c gprof.h
	cp lookup.c p.lookup.c
	cc -c -p ${CFLAGS} p.lookup.c
	rm p.lookup.c

p.printgprof.o: printgprof.c gprof.h
	cp printgprof.c p.printgprof.c
	cc -c -p ${CFLAGS} p.printgprof.c
	rm p.printgprof.c

p.calls.o: calls.c calls.h gprof.h
	cp calls.c p.calls.c
	cc -c -p ${CFLAGS} p.calls.c
	rm p.calls.c

LINTFLAGS = -x -a -n
lint:
	lint ${CFLAGS} ${LINTFLAGS} ${GPROFSRCS}

${GPROFHDRS} ${GPROFSRCS} ${OTHERS}:
	sccs get $@

sources: ${GPROFHDRS} ${GPROFSRCS} ${OTHERS}
