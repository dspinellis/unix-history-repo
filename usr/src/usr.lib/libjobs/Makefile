#	@(#)Makefile	4.5	(Berkeley)	2/10/83
#
CFLAGS=-O
# pseudo C code
PCOBJS=signal.o wait3.o
OBJS=	sigset.o ${PCOBJS}

libjobs.a: ${OBJS}
	cd profiled; ar cr ../libjobs_p.a `lorder ${OBJS} | tsort`
	ar cr libjobs.a `lorder ${OBJS} | tsort`

.c.o:
	cc -p ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out profiled/$*.o
	cc ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

${PCOBJS}: SYS.h
	/lib/cpp -E -DPROF $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out profiled/$*.o
	/lib/cpp -E $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out $*.o

install:
	install libjobs.a ${DESTDIR}/usr/lib
	ranlib ${DESTDIR}/usr/lib/libjobs.a
	install libjobs_p.a ${DESTDIR}/usr/lib
	ranlib ${DESTDIR}/usr/lib/libjobs_p.a

clean:
	rm -f libjobs.a libjobs_p.a *.o profiled/*.o
