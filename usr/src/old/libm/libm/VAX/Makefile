From Prof. Kahan at UC at Berkeley
MORE=atan2.o cbrt.o trig.o cabs.o support.o

.s.o:
####	insert additional code for profiling (sed -f VAX/mcount.sed)
	sed -f mcount.sed $*.s | ${AS} -o $*.o
	-ld -X -r $*.o
	mv a.out ../profiled/${MACH}/$*.o
	${AS} -o $*.o $*.s
	-ld -x -r $*.o
	mv a.out $*.o

all: ${MORE}
