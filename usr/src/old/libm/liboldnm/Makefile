#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
SCCSID = "%W (Berkeley) %G%"

CFLAGS=-O

INSTALL= install

FILES=	acos.o asin.o atan.o cbrt.o pow.o exp.o log.o sin.o \
	sinh.o sqrt.o tan.o tanh.o

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

.s.o:
	sed -f mcount.sed $*.s | ${AS} -o $*.o
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${AS} -o $*.o $*.s
	-ld -x -r $*.o
	mv a.out $*.o

libnm.a libnm_p.a : $(FILES)
	cd profiled; ar cru ../libnm_p.a $(FILES)
	ar cru libnm.a $(FILES)

install: libnm.a libnm_p.a
	${INSTALL} libnm.a ${DESTDIR}/usr/lib/libnm.a
	ranlib ${DESTDIR}/usr/lib/libnm.a
	${INSTALL} libnm_p.a ${DESTDIR}/usr/lib/libnm_p.a
	ranlib ${DESTDIR}/usr/lib/libnm_p.a

clean:
	rm -f *.o profiled/*.o libnm.a libnm_p.a
