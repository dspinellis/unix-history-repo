#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific written prior permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
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
# vax		- for the VAX D_floating format, the default.
# tahoe		- for the TAHOE double format.
# national	- for those IEEE machines whose floating point implementation
#		  has similar byte ordering as the NATIONAL 32016 with 32081.
# ieee		- for other IEEE machines, we hope.
#
CFLAGS=	-O -D${MACHINE}
LIBC=	/lib/libc.a
SUBDIR=	vax tahoe national common ieee
#
# Files comprising the standard Math library;
# actually there are more under ${MACHINE}/ subdirectory.
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

more.ieee: FRC
	for i in common ieee; do (cd $$i; make ${MFLAGS}); done

more.national: FRC
	for i in common national; do (cd $$i; make ${MFLAGS}); done

more.tahoe: FRC
	for i in common tahoe; do (cd $$i; make ${MFLAGS}); done

more.vax: FRC
	cd vax; make "CFLAGS=${CFLAGS}"

clean: FRC
	rm -f ${OBJS} core libm.a libm_p.a
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

depend: FRC
	mkdep ${CFLAGS} ${SRCS}
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

install: FRC
	install -o bin -g bin -m 644 libm.a ${DESTDIR}/usr/lib/libm.a
	ranlib ${DESTDIR}/usr/lib/libm.a
	install -o bin -g bin -m 644 libm_p.a ${DESTDIR}/usr/lib/libm_p.a
	ranlib ${DESTDIR}/usr/lib/libm_p.a

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} tags); done

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

acosh.o: acosh.c
asincos.o: asincos.c
asinh.o: asinh.c
atan.o: atan.c
atanh.o: atanh.c /usr/include/errno.h
cosh.o: cosh.c
erf.o: erf.c
exp.o: exp.c
exp__E.o: exp__E.c
expm1.o: expm1.c
floor.o: floor.c
lgamma.o: lgamma.c /usr/include/math.h /usr/include/errno.h
j0.o: j0.c /usr/include/math.h /usr/include/errno.h
j1.o: j1.c /usr/include/math.h /usr/include/errno.h
jn.o: jn.c /usr/include/math.h /usr/include/errno.h
log.o: log.c /usr/include/errno.h
log10.o: log10.c
log1p.o: log1p.c /usr/include/errno.h
log__L.o: log__L.c
pow.o: pow.c /usr/include/errno.h
sinh.o: sinh.c
tanh.o: tanh.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
