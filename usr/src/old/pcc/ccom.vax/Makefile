#	@(#)Makefile	1.27 (Berkeley) %G%

PROG=	ccom
MIP=	${.CURDIR}/../mip
CFLAGS+=-DONEPASS -I. -I${.CURDIR} -I${MIP}
SRCS=	allo.c code.c common.c local.c local2.c match.c optim.c order.c \
	pftn.c reader.c rel.c scan.c stab.c table.c trees.c xdefs.c
OBJS+=	cgram.o rodata.o
NOMAN=	noman
.PATH:	${MIP}
CLEANFILES+=cgram.c rodata.c pcctokens pcclocal.h

rodata.c cgram.c: ${MIP}/cgram.y pcctokens
	cat pcctokens ${MIP}/cgram.y | ${YACC} /dev/stdin
	yyfix rodata.c
	mv y.tab.c cgram.c

common.o:
	${CC} ${CFLAGS} -DPASS1COMMON -c ${.IMPSRC}

rodata.o table.o:
	${CC} ${CFLAGS} -c -R ${.IMPSRC}

pcclocal.h: localdefs.h /usr/include/pcc.h
	cat ${.ALLSRC} | \
	    egrep '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | \
	    sed -e 's/PCC[A-Z]*_//' > ${.TARGET}

pcctokens: localdefs.h /usr/include/pcc.h
	cat ${.ALLSRC} | \
	    egrep '^#[ 	]*define[ 	][ 	]*PCC_' | \
	    sed -e 's/^#[ 	]*define[ 	][ 	]*PCC_/%term	/' > \
	    ${.TARGET}

${OBJS} .depend: pcclocal.h pcctokens

.include <bsd.prog.mk>
