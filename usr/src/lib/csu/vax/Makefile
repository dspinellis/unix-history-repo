# @(#)Makefile	4.3 (Berkeley) %G%
#
#	crt0	Normal C run time startoff
#	mcrt0	C run time start off for profiling, ``prof'' conventions
#	gcrt0	C run time start off for profiling, ``gprof'' conventions
#
OBJS=	crt0.o mcrt0.o gcrt0.o

all: ${OBJS}

install: ${OBJS}
	mv crt0.o /lib/crt0.o
	mv mcrt0.o /lib/mcrt0.o
	mv gcrt0.o /usr/lib/gcrt0.o

crt0.s:	cmgt.crt0.c
	cc -S ${DFLAGS} -DCRT0 cmgt.crt0.c
	mv cmgt.crt0.s crt0.s
mcrt0.s: cmgt.crt0.c
	cc -S ${DFLAGS} -DMCRT0 cmgt.crt0.c
	mv cmgt.crt0.s mcrt0.s
gcrt0.s: cmgt.crt0.c gcrt0.h
	cc -S ${DFLAGS} -DGCRT0 cmgt.crt0.c
	mv cmgt.crt0.s gcrt0.s

.s.o: crt0.ex
	ex - $*.s < crt0.ex
	/lib/cpp $*.s > $*.S
	mv $*.S $*.s
	as -o $*.o $*.s
	ld -x -r $*.o
	mv a.out $*.o
	rm -f $*.s
clean:
	rm -f ${OBJS}

lint:	cmgt.crt0.c
	lint cmgt.crt0.c -DMCRT0
	lint cmgt.crt0.c -DGCRT0
	lint cmgt.crt0.c -DCRT0
