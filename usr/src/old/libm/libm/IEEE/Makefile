From Prof. Kahan at UC at Berkeley
FILES=atan2.o cbrt.o trig.o cabs.o support.o

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out ../profiled/${MACH}/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

all: ${FILES}
