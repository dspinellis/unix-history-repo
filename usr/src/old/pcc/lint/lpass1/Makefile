#	@(#)Makefile	5.3 (Berkeley) %G%

MIP=	${.CURDIR}/../../../libexec/pcc/mip

PROG=	lintpass1
CFLAGS+=-I${MIP} -I. -I${.CURDIR}
.PATH:	${MIP}
SRCS=	cgram.c common.c hash.c lint.c optim.c pftn.c rel.c rodata.c scan.c \
	trees.c xdefs.c
BINDIR=	/usr/libexec
MAN1=	lint.0

.include <bsd.prog.mk>

rodata.o:
	$(CC) -c ${CFLAGS} -R ${.IMPSRC}

common.o:
	${CC} -c ${CFLAGS} -DPASS1COMMON ${.IMPSRC}

rodata.c cgram.c: pcclocal.h pcctokens
	cat pcctokens ${MIP}/cgram.y > gram.in
	${YACC} gram.in
	yyfix rodata.c yylhs yylen yydefred yydgoto yysindex yyrindex yygindex \
	    yytable yycheck
	mv y.tab.c cgram.c

pcclocal.h: localdefs.h /usr/include/pcc.h
	cat ${.ALLSRC} | \
	   egrep '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | \
	   sed -e 's/PCC[A-Z]*_//' > ${.TARGET}

pcctokens: localdefs.h /usr/include/pcc.h
	cat ${.ALLSRC} | \
	    egrep '^#[ 	]*define[ 	][ 	]*PCC_' | \
	    sed -e 's/^#[ 	]*define[ 	][ 	]*PCC_/%term	/' > \
	    ${.TARGET}
