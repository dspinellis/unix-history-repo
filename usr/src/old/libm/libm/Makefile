#
#	@(#)Makefile	4.6	%G%
#
SCCSID = "@(#)Makefile 4.6 %G%"
#
# This high quality math library is intended to run on either a VAX in
# D_floating format or a machine that conforms to the IEEE standard 754
# for double precision floating-point arithmetic.
#
# WARNING: On machines other than the ones mentioned above, run the original
# Version 7 math library, if nothing better is available.

#
# MACH indicates the type of floating point hardware you are using; legal
# values are:
#
# VAX		- for the VAX D_floating format, the default.
# NATIONAL	- for those IEEE machines whose floating point implementation
#		  has similar byte ordering as the NATIONAL 32016 with 32081.
# IEEE		- for other IEEE machines, we hope.
#
MACH=VAX

#
# invoke object-code optimizer with appropriate MACH definition
#
CFLAGS=-O -D${MACH}

INSTALL=install

DESTDIR=

#
# files comprising the standard Math library
#
SRCS=	E.c L.c acosh.c asinh.c atanh.c atrig.c cosh.c \
	erf.c exp.c exp__E.c floor.c gamma.c j0.c j1.c jn.c \
	log.c log10.c log__L.c pow.c sinh.c tanh.c
FILES=cosh.o sinh.o tanh.o acosh.o asinh.o atanh.o atrig.o \
E.o L.o exp.o log.o log10.o pow.o exp__E.o log__L.o \
jn.o j0.o j1.o gamma.o erf.o floor.o
MORE=\
${MACH}/atan2.o ${MACH}/cbrt.o ${MACH}/trig.o ${MACH}/cabs.o ${MACH}/support.o
TAGSFILE=tags

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

libm.a libm_p.a : ${FILES}
	cd ${MACH}; make "MACH=${MACH}" "CFLAGS=${CFLAGS}"
	cd profiled; ar cru ../libm_p.a ${FILES} ${MORE}
	ar cru libm.a ${FILES} ${MORE}

install: libm.a libm_p.a
	-rm -f ${DESTDIR}/usr/lib/libnm.a ${DESTDIR}/usr/lib/libnm_p.a
	${INSTALL} libm.a ${DESTDIR}/usr/lib
	ln ${DESTDIR}/usr/lib/libm.a ${DESTDIR}/usr/lib/libnm.a
	ranlib ${DESTDIR}/usr/lib/libm.a
	${INSTALL} libm_p.a ${DESTDIR}/usr/lib
	ln ${DESTDIR}/usr/lib/libm_p.a ${DESTDIR}/usr/lib/libnm_p.a
	ranlib ${DESTDIR}/usr/lib/libm_p.a

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	-rm -f *.o ${MACH}/*.o profiled/*.o profiled/${MACH}/*.o \
	    libm.a libm_p.a tags
