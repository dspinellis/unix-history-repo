SCCSID = @(#)Makefile	1.2 (Berkeley) %G%
RRHBIN = /va/staff/rrh/bin
NCC = ${RRHBIN}/ncc

CFLAGS = -DDEBUG

DPROFHDRS =	dprof.h
DPROFSRCS =	dprof.c arcs.c dfn.c lookup.c printdprof.c
DPROFOBJS =	dprof.o arcs.o dfn.o lookup.o printdprof.o
OTHERS =	mcrt0.s monitor.h monitor.c monitor.ex

dprof: $(DPROFOBJS)
	cc -o dprof ${CFLAGS} $(DPROFOBJS)

.c.o:
	cc -c ${CFLAGS} $*.c

mcrt0.o: mcrt0.s
	as -o mcrt0.o mcrt0.s
	-ld -x -r mcrt0.o
	mv a.out mcrt0.o

monitor.o: monitor.s
	as -o monitor.o monitor.s

monitor.s: monitor.c monitor.ex monitor.h
	cc -S -DMONITOR monitor.c
	ex - monitor.s < monitor.ex
	/lib/cpp monitor.s > monitor.S
	mv monitor.S monitor.s

#
#	NB: -x option to ncc is like -p, but uses
#	different files to load at the beginning of
#	the executable.
#
profit: profit.o monitor.o mcrt0.o
	/bin/ld -X mcrt0.o monitor.o -o profit profit.o -lcp
#	${NCC} -x -p "-?" profit.o -o profit

profit.o:
	${NCC} -x -p -c "-?" profit.c

dprof.o: dprof.c dprof.h
arcs.o: arcs.c dprof.h
lookup.o: lookup.c dprof.h
dfn.o: dfn.c dprof.h
printdprof.o: printdprof.c dprof.h

PDPROF = p.dprof.o p.arcs.o p.dfn.o p.lookup.o p.printdprof.o

p.dprof: ${PDPROF}
	/bin/ld -X mcrt0.o monitor.o -o p.dprof ${PDPROF} -lcp

p.dprof.o: dprof.c dprof.h
	ln dprof.c p.dprof.c
	cc -c -p ${CFLAGS} p.dprof.c
	rm p.dprof.c

p.arcs.o: arcs.c dprof.h
	ln arcs.c p.arcs.c
	cc -c -p ${CFLAGS} p.arcs.c
	rm p.arcs.c

p.dfn.o: dfn.c dprof.h
	ln dfn.c p.dfn.c
	cc -c -p ${CFLAGS} p.dfn.c
	rm p.dfn.c

p.lookup.o: lookup.c dprof.h
	ln lookup.c p.lookup.c
	cc -c -p ${CFLAGS} p.lookup.c
	rm p.lookup.c

p.printdprof.o: printdprof.c dprof.h
	ln printdprof.c p.printdprof.c
	cc -c -p ${CFLAGS} p.printdprof.c
	rm p.printdprof.c

LINTFLAGS = -x -a -n
lint:
	lint ${CFLAGS} ${LINTFLAGS} dprof.c arcs.c dfn.c lookup.c printdprof.c

${DPROFHDRS} ${DPROFSRCS} ${OTHERS}:
	sccs get $@

sources: ${DPROFHDRS} ${DPROFSRCS} ${OTHERS}
