#	Makefile	4.4	83/07/02
#
OBJS=	_setjmp.o \
	abort.o abs.o alloca.o atof.o \
	bcmp.o bcopy.o bzero.o \
	disktab.o \
	ecvt.o \
	fabs.o ffs.o frexp.o \
	index.o insque.o \
	ldexp.o \
	modf.o \
	nargs.o \
	remque.o rindex.o \
	setjmp.o strcat.o strcmp.o strcpy.o strlen.o strncat.o \
	strncmp.o strncpy.o \
	udiv.o urem.o
CFLAGS=	-O

.s.o:
	/lib/cpp -E -DPROF $*.s | ${AS} -o $*.o
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	/lib/cpp -E $*.s | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out $*.o

.c.o:
	cc -p -c ${CFLAGS} $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	cc -c ${CFLAGS} $*.c
	-ld -x -r $*.o
	mv a.out $*.o

genlib genlib_p: ${OBJS}
	@echo "building profiled genlib"
	@cd profiled; ar cru ../genlib_p ${OBJS}
	@echo "building normal genlib"
	@ar cru genlib ${OBJS}

clean:
	rm -f ${OBJS} profiled/*.o errs a.out core genlib genlib_p
