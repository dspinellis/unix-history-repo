#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	1.11 (Berkeley) %G%
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
	exp.c exp__E.c expm1.c floor.c fmod.c lgamma.c j0.c j1.c jn.c \
	log.c log10.c log1p.c log__L.c pow.c sinh.c tanh.c
OBJS=	acosh.o asincos.o asinh.o atan.o atanh.o cosh.o erf.o \
	exp.o exp__E.o expm1.o floor.o fmod.o lgamma.o j0.o j1.o jn.o \
	log.o log10.o log1p.o log__L.o pow.o sinh.o tanh.o 
MAN=	abs.0 asinh.0 erf.0 exp.0 floor.0 hypot.0 ieee.0 infnan.0 j0.0 \
	lgamma.0 math.0 sin.0 sinh.0 sqrt.0
MDIR=	${DESTDIR}/usr/man/cat3

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
	@echo building normal libm
	@ar cru libm.a ${OBJS}
	ranlib libm.a
	@echo building profiled libm
	@cd profiled; ar cru ../libm_p.a ${OBJS}
	ranlib libm_p.a

more.ieee: FRC
	for i in common ieee; do (cd $$i; make ${MFLAGS}); done

more.national: FRC
	for i in common national; do (cd $$i; make ${MFLAGS}); done

more.tahoe: FRC
	for i in common tahoe; do (cd $$i; make ${MFLAGS}); done

more.vax: FRC
	cd vax; make "CFLAGS=${CFLAGS}"

clean: 
	rm -f ${OBJS} profiled/*.o core libm.a libm_p.a
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

cleandir: clean
	rm -f ${MAN} tags .depend

depend: 
	mkdep ${CFLAGS} ${SRCS}
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done

install: ${MAN}
	install -o bin -g bin -m 444 libm.a ${DESTDIR}/usr/lib/libm.a
	ranlib -t ${DESTDIR}/usr/lib/libm.a
	install -o bin -g bin -m 444 libm_p.a ${DESTDIR}/usr/lib/libm_p.a
	ranlib -t ${DESTDIR}/usr/lib/libm_p.a
	install -c -o bin -g bin -m 444 ${MAN} ${MDIR}
	rm -f ${MDIR}/acosh.0; ln ${MDIR}/asinh.0 ${MDIR}/acosh.0
	rm -f ${MDIR}/atanh.0; ln ${MDIR}/asinh.0 ${MDIR}/atanh.0
	rm -f ${MDIR}/erfc.0; ln ${MDIR}/erf.0 ${MDIR}/erfc.0
	rm -f ${MDIR}/expm1.0; ln ${MDIR}/exp.0 ${MDIR}/expm1.0
	rm -f ${MDIR}/log.0; ln ${MDIR}/exp.0 ${MDIR}/log.0
	rm -f ${MDIR}/log10.0; ln ${MDIR}/exp.0 ${MDIR}/log10.0
	rm -f ${MDIR}/log1p.0; ln ${MDIR}/exp.0 ${MDIR}/log1p.0
	rm -f ${MDIR}/pow.0; ln ${MDIR}/exp.0 ${MDIR}/pow.0
	rm -f ${MDIR}/fabs.0; ln ${MDIR}/floor.0 ${MDIR}/fabs.0
	rm -f ${MDIR}/ceil.0; ln ${MDIR}/floor.0 ${MDIR}/ceil.0
	rm -f ${MDIR}/rint.0; ln ${MDIR}/floor.0 ${MDIR}/rint.0
	rm -f ${MDIR}/cabs.0; ln ${MDIR}/hypot.0 ${MDIR}/cabs.0
	rm -f ${MDIR}/copysign.0; ln ${MDIR}/ieee.0 ${MDIR}/copysign.0
	rm -f ${MDIR}/drem.0; ln ${MDIR}/ieee.0 ${MDIR}/drem.0
	rm -f ${MDIR}/finite.0; ln ${MDIR}/ieee.0 ${MDIR}/finite.0
	rm -f ${MDIR}/logb.0; ln ${MDIR}/ieee.0 ${MDIR}/logb.0
	rm -f ${MDIR}/scalb.0; ln ${MDIR}/ieee.0 ${MDIR}/scalb.0
	rm -f ${MDIR}/j1.0; ln ${MDIR}/j0.0 ${MDIR}/j1.0
	rm -f ${MDIR}/jn.0; ln ${MDIR}/j0.0 ${MDIR}/jn.0
	rm -f ${MDIR}/y0.0; ln ${MDIR}/j0.0 ${MDIR}/y0.0
	rm -f ${MDIR}/y1.0; ln ${MDIR}/j0.0 ${MDIR}/y1.0
	rm -f ${MDIR}/yn.0; ln ${MDIR}/j0.0 ${MDIR}/yn.0
	rm -f ${MDIR}/gamma.0; ln ${MDIR}/lgamma.0 ${MDIR}/gamma.0
	rm -f ${MDIR}/tan.0; ln ${MDIR}/sin.0 ${MDIR}/tan.0
	rm -f ${MDIR}/acos.0; ln ${MDIR}/sin.0 ${MDIR}/acos.0
	rm -f ${MDIR}/asin.0; ln ${MDIR}/sin.0 ${MDIR}/asin.0
	rm -f ${MDIR}/atan.0; ln ${MDIR}/sin.0 ${MDIR}/atan.0
	rm -f ${MDIR}/atan2.0; ln ${MDIR}/sin.0 ${MDIR}/atan2.0
	rm -f ${MDIR}/cos.0; ln ${MDIR}/sin.0 ${MDIR}/cos.0
	rm -f ${MDIR}/cosh.0; ln ${MDIR}/sinh.0 ${MDIR}/cosh.0

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
	for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} tags); done

FRC:
