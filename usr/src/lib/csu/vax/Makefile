# @(#)Makefile	1.2 (Berkeley) %G%

OBJS=	crt0.o mcrt0.o gcrt0.o

.c.o:
	cc -S ${DFLAGS} $*.c
	ex - $*.s < crt0.ex
	/lib/cpp $*.s > $*.S
	mv $*.S $*.s
	as -o $*.o $*.s
	ld -x -r $*.o
	mv a.out $*.o
	rm -f $*.s

all: ${OBJS}

clean:
	rm -f ${OBJS}

${OBJS}: crt0.ex
gcrt0.o: gcrt0.h
