# @(#)Makefile	4.6 (Berkeley) %G%
#
#	crt0	Normal C run time startoff
#	mcrt0	C run time start off for profiling, ``prof'' conventions
#	gcrt0	C run time start off for profiling, ``gprof'' conventions
#
DESTDIR=
OBJS=	crt0.o mcrt0.o gcrt0.o mon.o gmon.o

all: ${OBJS}

install: ${OBJS}
	mv crt0.o ${DESTDIR}/lib/crt0.o
	mv mcrt0.o ${DESTDIR}/lib/mcrt0.o
	mv gcrt0.o ${DESTDIR}/usr/lib/gcrt0.o

crt0.o:	crt0.c
	cc -S ${DFLAGS} -DCRT0 crt0.c
	/lib/cpp crt0.s > x.s
	as -o x.o x.s
	ld -x -r -o crt0.o x.o
	rm -f x.s x.o crt0.s

moncrt0.o: crt0.c
	cc -S ${DFLAGS} -DMCRT0 crt0.c
	/lib/cpp crt0.s > x.s
	as -o x.o x.s
	ld -x -r -o moncrt0.o x.o
	rm -f x.s x.o crt0.s

gcrt0.o: moncrt0.o gmon.o
	ld -x -r -o gcrt0.o moncrt0.o gmon.o

mcrt0.o: moncrt0.o mon.o
	ld -x -r -o mcrt0.o moncrt0.o mon.o

mon.o: mon.c mcount.ex
	cc -S ${DFLAGS} mon.c
	ex - mon.s < mcount.ex
	/lib/cpp mon.s > x.s
	as -o x.o x.s
	ld -x -r -o mon.o x.o
	rm -f x.s x.o mon.s

gmon.o: gmon.c gmon.h mcount.ex
	cc -S ${DFLAGS} gmon.c
	ex - gmon.s < mcount.ex
	/lib/cpp gmon.s > x.s
	as -o x.o x.s
	ld -x -r -o gmon.o x.o
	rm -f x.s x.o gmon.s

clean:
	rm -f ${OBJS} *.o *.s core errs

lint:	crt0.c
	lint crt0.c -DMCRT0
	lint crt0.c -DGCRT0
	lint crt0.c -DCRT0
