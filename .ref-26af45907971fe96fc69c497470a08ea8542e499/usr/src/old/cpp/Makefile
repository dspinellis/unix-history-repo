#	@(#)Makefile	5.3 (Berkeley) %G%

PROG=	cpp
CFLAGS+=-Dunix=1 -DFLEXNAMES -I${.CURDIR}
SRCS=	cpp.c cpy.c rodata.o
CLEANFILES+=cpy.c rodata.c
NOMAN=	noman

rodata.o: cpy.c
	${CC} ${CFLAGS} -R -c ${.PREFIX}.c

cpy.c: cpy.y
	${YACC} ${.ALLSRC}
	yyfix rodata.c
	mv y.tab.c cpy.c

.include <bsd.prog.mk>
