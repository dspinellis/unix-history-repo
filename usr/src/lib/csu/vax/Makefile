# @(#)Makefile	1.1 (Berkeley) %G%

OBJS=	crt0.o mcrt0.o gcrt0.o

.s.o:
	${AS} -o $*.o $*.s
	-ld -x -r $*.o
	mv a.out $*.o

all: ${OBJS}

gcrt0.o: gcrt0.c gcrt0.h gcrt0.ex
	cc -S ${DFLAGS} gcrt0.c
	ex - gcrt0.s < gcrt0.ex
	/lib/cpp gcrt0.s > gcrt0.S
	mv gcrt0.S gcrt0.s
	as -o gcrt0.o gcrt0.s
	rm -f gcrt0.s

clean:
	rm -f ${OBJS}
