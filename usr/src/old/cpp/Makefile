#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	cpp
CFLAGS+=-Dunix=1 -DFLEXNAMES -I${.CURDIR}
YYFIX=	${.CURDIR}/../../libexec/pcc/pcc.${MACHINE}/:yyfix
SRCS=	cpp.c cpy.c rodata.o
CLEANFILES+=	cpy.c rodata.c
NOMAN=	noman

rodata.o: cpy.c
	${CC} ${CFLAGS} -R -c ${.PREFIX}.c

cpy.c: cpy.y
	${YACC} ${.ALLSRC}
	${YYFIX} yylhs yylen yydefred yydgoto yysindex yyrindex \
	    yygindex yytable yycheck
	mv y.tab.c cpy.c

.include <bsd.prog.mk>
