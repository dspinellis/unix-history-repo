PROG=expr
SRCS=expr
CFLAGS+=-I. -I${.CURDIR}
NOMAN=noman
CLEANFILES+=expr.c y.tab.h

expr.c:
	${YACC} -d ${.IMPSRC}
	mv y.tab.c expr.c

.include <bsd.prog.mk>

