#
#	@(#)Makefile	4.10	%G%
#
SCCSID = "@(#)Makefile	4.10 %G%"
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

DESTDIR=
MACHINE=unknown
CFLAGS=-O

INSTALL=install

#
# Files comprising the standard Math library;
# actually there are more under ${MACH}/ subdirectory.
#
SRCS =	acosh.c asincos.c asinh.c atan.c atanh.c cosh.c erf.c \
	exp.c exp__E.c expm1.c floor.c lgamma.c j0.c j1.c jn.c \
	log.c log10.c log1p.c log__L.c pow.c sinh.c tanh.c

FILES =	acosh.o asincos.o asinh.o atan.o atanh.o cosh.o erf.o \
	exp.o exp__E.o expm1.o floor.o lgamma.o j0.o j1.o jn.o \
	log.o log10.o log1p.o log__L.o pow.o sinh.o tanh.o 

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

all: libm.a libm_p.a

libm.a libm_p.a: ${FILES} more.${MACHINE}
	cd profiled; ar cru ../libm_p.a ${FILES}
	ar cru libm.a ${FILES}

more.vax:
	@cd VAX; make "CFLAGS=${CFLAGS} -DVAX"

more.tahoe:
	@cd IEEE; make "CFLAGS=${CFLAGS} -DTAHOE"

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
	-rm -f *.o IEEE/*.o VAX/*.o profiled/*.o libm.a libm_p.a tags
