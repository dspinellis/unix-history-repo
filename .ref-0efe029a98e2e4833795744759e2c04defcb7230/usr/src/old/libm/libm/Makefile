#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.12	(Berkeley)	%G%
#
# This high quality math library is intended to run on either a VAX in
# D_floating format or a machine that conforms to the IEEE standard 754
# for double precision floating-point arithmetic.
#
# WARNING: On machines other than the ones mentioned above, run the original
# Version 7 math library, if nothing better is available.
#
# MACHINE indicates the type of floating point hardware you are using; legal
# values are:
#
# VAX		- for the VAX D_floating format, the default.
# NATIONAL	- for those IEEE machines whose floating point implementation
#		  has similar byte ordering as the NATIONAL 32016 with 32081.
# IEEE		- for other IEEE machines, we hope.
#
CFLAGS=	-O
LIBC=	/lib/libc.a
#
# Files comprising the standard Math library;
# actually there are more under ${MACH}/ subdirectory.
#
SRCS=	acosh.c asincos.c asinh.c atan.c atanh.c cosh.c erf.c \
	exp.c exp__E.c expm1.c floor.c lgamma.c j0.c j1.c jn.c \
	log.c log10.c log1p.c log__L.c pow.c sinh.c tanh.c
OBJS=	acosh.o asincos.o asinh.o atan.o atanh.o cosh.o erf.o \
	exp.o exp__E.o expm1.o floor.o lgamma.o j0.o j1.o jn.o \
	log.o log10.o log1p.o log__L.o pow.o sinh.o tanh.o 

.c.o:
####	generate additional code for profiling (-p)
	${CC} -p ${CFLAGS} -c $*.c
####	generate relocation bits (-r) & preserve symbols that begin with L (-X)
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
####	generate relocation bits (-r) but don't preserve local symbols (-x)
	-ld -x -r $*.o
	mv a.out $*.o

all: libm.a libm_p.a

libm.a libm_p.a: ${OBJS} more.${MACHINE}
	cd profiled; ar cru ../libm_p.a ${OBJS}
	ar cru libm.a ${OBJS}

more.vax: FRC
	cd VAX; make "CFLAGS=${CFLAGS} -DVAX"

more.tahoe: FRC
	cd IEEE; make "CFLAGS=${CFLAGS} -DTAHOE"

clean: FRC
	rm -f ${OBJS} core libnm.a libnm_p.a IEEE/*.o VAX/*.o profiled/*.o

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	-rm -f ${DESTDIR}/usr/lib/libnm.a ${DESTDIR}/usr/lib/libnm_p.a
	install -o bin -g bin -m 644 libm.a ${DESTDIR}/usr/lib/libm.a
#	ln ${DESTDIR}/usr/lib/libm.a ${DESTDIR}/usr/lib/libnm.a
	ranlib ${DESTDIR}/usr/lib/libm.a
	install -o bin -g bin -m 644 libm_p.a ${DESTDIR}/usr/lib/libm_p.a
#	ln ${DESTDIR}/usr/lib/libm_p.a ${DESTDIR}/usr/lib/libnm_p.a
	ranlib ${DESTDIR}/usr/lib/libm_p.a

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

acosh.o: acosh.c
asincos.o: asincos.c
asinh.o: asinh.c
atan.o: atan.c
atanh.o: atanh.c
cosh.o: cosh.c
erf.o: erf.c
exp.o: exp.c
exp__E.o: exp__E.c
expm1.o: expm1.c
floor.o: floor.c
lgamma.o: lgamma.c /usr/include/math.h
j0.o: j0.c /usr/include/math.h
j1.o: j1.c /usr/include/math.h
jn.o: jn.c /usr/include/math.h
log.o: log.c
log10.o: log10.c
log1p.o: log1p.c
log__L.o: log__L.c
pow.o: pow.c
sinh.o: sinh.c
tanh.o: tanh.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
