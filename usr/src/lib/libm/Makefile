#	@(#)Makefile	1.13 (Berkeley) %G%
#
# This high quality math library is intended to run on either a VAX in
# D_floating format or a machine that conforms to the IEEE standard 754
# for double precision floating-point arithmetic.
#
# WARNING: On machines other than the ones mentioned above, run the original
# Version 7 math library, if nothing better is available.
#
# HARDWARE indicates the type of floating point hardware you are using;
# we use MACHINE because we don't have anything better.
# Current legal values are:
#
# vax		- for the VAX D_floating format, the default.
# tahoe		- for the TAHOE double format.
# national	- for those IEEE machines whose floating point implementation
#		  has similar byte ordering as the NATIONAL 32016 with 32081.
# ieee		- for other IEEE machines, we hope.

LIB=	m
HARDWARE=${MACHINE}
CFLAGS+=-D${HARDWARE} -I${.CURDIR}/common_source
.PATH:	${.CURDIR}/common_source

SRCS=	acosh.c asincos.c asinh.c atan.c atanh.c cosh.c erf.c exp.c \
	exp__E.c expm1.c floor.c fmod.c lgamma.c j0.c j1.c jn.c log.c \
	log10.c log1p.c log__L.c pow.c sinh.c tanh.c

MAN3+=	asinh.0 erf.0 exp.0 floor.0 hypot.0 ieee.0 infnan.0 j0.0 \
	lgamma.0 math.0 sin.0 sinh.0 sqrt.0

MLINKS+=asinh.3 acosh.3 asinh.3 atanh.3
MLINKS+=erf.3 erfc.3
MLINKS+=exp.3 expm1.3 exp.3 log.3 exp.3 log10.3 exp.3 log1p.3 exp.3 pow.3
MLINKS+=floor.3 fabs.3 floor.3 ceil.3 floor.3 rint.3
MLINKS+=hypot.3 cabs.3
MLINKS+=ieee.3 copysign.3 ieee.3 drem.3 ieee.3 finite.3 ieee.3 logb.3 \
	ieee.3 scalb.3
MLINKS+=j0.3 j1.3 j0.3 jn.3 j0.3 y0.3 j0.3 y1.3 j0.3 yn.3
MLINKS+=lgamma.3 gamma.3
MLINKS+=sin.3 tan.3 sin.3 acos.3 sin.3 asin.3 sin.3 atan.3 sin.3 atan2.3 \
	sin.3 cos.3
MLINKS+=sinh.3 cosh.3

.if $(HARDWARE) == "ieee"
.PATH:	${.CURDIR}/common ${.CURDIR}/ieee
SRCS+=	atan2.c sincos.c tan.c				# common
SRCS+=	cabs.c cbrt.c support.c				# ieee
.endif

.if $(HARDWARE) == "national"
.PATH:	${.CURDIR}/common ${.CURDIR}/national
SRCS+=	atan2.c sincos.c tan.c				# common
SRCS+=	cabs.c cbrt.c sqrt.s support.s			# national
.endif

.if $(HARDWARE) == "tahoe"
.PATH:	${.CURDIR}/common ${.CURDIR}/tahoe
SRCS+=	atan2.c sincos.c tan.c				# common
SRCS+=	cabs.s cbrt.s sqrt.s support.s infnan.s		# tahoe
.endif

.if $(HARDWARE) == "vax"
.PATH:	${.CURDIR}/vax
SRCS+=	atan2.s cabs.s cbrt.s sqrt.s sincos.s tan.s \
	argred.s support.s infnan.s			# vax
.endif

# can't use the standard mkdep, because there are some .s files that
# are using '#' as a comment indicator and cpp thinks it's an undefined
# control.

depend: ${SRCS}
	mkdep ${CFLAGS:M-[ID]*} ${.ALLSRC:M*.c}

.include <bsd.lib.mk>

.s.o:
	${AS} -o ${.TARGET} ${.IMPSRC}
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

.s.po:
	sed -f ${.CURDIR}/${HARDWARE}/mcount.sed ${.IMPSRC} | \
	    ${AS} -o ${.TARGET}
	@${LD} -X -r ${.TARGET}
	@mv a.out ${.TARGET}

