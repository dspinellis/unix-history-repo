#        @(#)Makefile	1.4 (Berkeley) %G%


PROG=	test
SRCS=	test.c error.c stalloc.c operators.c
CFLAGS += -I{.CURDIR} -I.
CLEANFILES+= operators.c operators.h unary_op binary_op

.PATH: /usr/src/bin/test/obj

afterinstall:
	-rm -f '['
	ln test '['

operators.c operators.h: unary_op binary_op mkops
	cp ${.CURDIR}/unary_op ${.CURDIR}/binary_op .
	sh ${.CURDIR}/mkops

.include <bsd.prog.mk>
