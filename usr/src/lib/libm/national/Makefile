# @(#)Makefile	1.3 (ucb.elefunt) %G%
CFLAGS= -DNATIONAL -O
SRCS=cbrt.c sqrt.s support.s
OBJS=cbrt.o sqrt.o support.o

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out ../profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

.s.o:
####	insert additional code for profiling
	sed -f mcount.sed $*.s | ${AS} -o $*.o
	-ld -X -r $*.o
	mv a.out ../profiled/$*.o
	${AS} -o $*.o $*.s
	-ld -x -r $*.o
	mv a.out $*.o

all: ../libm.a ../libm_p.a

../libm.a ../libm_p.a: ${OBJS}
	cd ../profiled; ar cru ../libm_p.a ${OBJS}
	ar cru ../libm.a ${OBJS}
