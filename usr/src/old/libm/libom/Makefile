#	@(#)Makefile	4.3	%G%
#
SCCSID = "@(#)Makefile 4.3 %G%"

CFLAGS=-O

INSTALL= install

FILES= asin.o atan.o hypot.o jn.o j0.o j1.o pow.o gamma.o \
	log.o sin.o sqrt.o tan.o tanh.o sinh.o erf.o exp.o floor.o

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

libm.a libm_p.a : $(FILES)
	cd profiled; ar cru ../libm_p.a $(FILES)
	ar cru libm.a $(FILES)

install: libm.a libm_p.a
	${INSTALL} libm.a ${DESTDIR}/usr/lib
	ranlib ${DESTDIR}/usr/lib/libm.a
	${INSTALL} libm_p.a ${DESTDIR}/usr/lib
	ranlib ${DESTDIR}/usr/lib/libm_p.a

clean:
	rm -f *.o profiled/*.o libm.a libm_p.a
