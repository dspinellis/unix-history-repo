# @(#)Makefile	1.4 (ucb.elefunt) %G%
MORE=atan2.o cabs.o cbrt.o sqrt.o sincos.o tan.o argred.o support.o infnan.o

.s.o:
####	insert additional code for profiling
	sed -f mcount.sed $*.s | ${AS} -o $*.o
	-ld -X -r $*.o
	mv a.out ../profiled/$*.o
	${AS} -o $*.o $*.s
	-ld -x -r $*.o
	mv a.out $*.o

all: ../libm.a ../libm_p.a

../libm_p.a ../libm.a: ${MORE}
	cd ../profiled; ar cru ../libm_p.a ${MORE}
	ar cru ../libm.a ${MORE}
