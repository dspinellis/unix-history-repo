#        @(#)Makefile	1.5 (Berkeley) %G%


PROG=	test
PROG2=  '['
SRCS=   test.c error.c stalloc.c operators.c
CFLAGS += -I{.CURDIR} -I.
CLEANFILES+= operators.c operators.h unary_op binary_op

.PATH: /usr/src/bin/test/obj

afterinstall:
	ln ${DESTDIR}${BINDIR}/${PROG} ${DESTDIR}${BINDIR}/${PROG2}

operators.c operators.h: unary_op binary_op mkops
	cp ${.CURDIR}/unary_op ${.CURDIR}/binary_op .
	sh ${.CURDIR}/mkops


operators.o: operators.c operators.h

.include <bsd.prog.mk>
