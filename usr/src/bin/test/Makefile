#        @(#)Makefile	5.1 (Berkeley) %G%

PROG=	test
SRCS=   test.c operators.c
CFLAGS+=-I.
LINKS=	${BINDIR}/test ${BINDIR}/[
MLINKS=	test.1 '[.1'
CLEANFILES+= operators.c operators.h unary_op binary_op

operators.c operators.h: unary_op binary_op mkops
	cp ${.CURDIR}/unary_op ${.CURDIR}/binary_op .
	sh ${.CURDIR}/mkops

.include <bsd.prog.mk>
