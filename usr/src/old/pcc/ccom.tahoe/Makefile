#	@(#)Makefile	1.15 (Berkeley) %G%

PROG=	ccom
MIP=	${.CURDIR}/../mip
CFLAGS+=-DONEPASS -I. -I${.CURDIR} -I${MIP}
SRCS=	allo.c code.c common.c local.c local2.c match.c optim.c order.c \
	pftn.c reader.c rel.c scan.c stab.c table.c trees.c xdefs.c
OBJS+=	cgram.o rodata.o
NOMAN=	noman
.PATH:	${MIP}
CLEANFILES+=cgram.c gram.in rodata.c pcctokens pcclocal.h

cgram.o: cgram.c
	${CC} ${CFLAGS} -c ${.ALLSRC}

rodata.c cgram.c: ${MIP}/cgram.y pcctokens
	cat pcctokens ${MIP}/cgram.y > gram.in
	${YACC} gram.in
	yyfix rodata.c
	mv y.tab.c cgram.c

common.o: ${MIP}/common.c
	${CC} ${CFLAGS} -DPASS1COMMON -c ${.ALLSRC}

rodata.o: rodata.c
	${CC} ${CFLAGS} -c -R rodata.c

table.o: ${.CURDIR}/${.PREFIX}.c
	${CC} ${CFLAGS} -c -R ${.CURDIR}/${.PREFIX}.c

pcclocal.h: ${.CURDIR}/../ccom.vax/localdefs.h /usr/include/pcc.h
	cat ${.ALLSRC} | \
	    egrep '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | \
	    sed -e 's/PCC[A-Z]*_//' > ${.TARGET}

pcctokens: ${.CURDIR}/../ccom.vax/localdefs.h /usr/include/pcc.h
	cat ${.ALLSRC} | \
	    egrep '^#[ 	]*define[ 	][ 	]*PCC_' | \
	    sed -e 's/^#[ 	]*define[ 	][ 	]*PCC_/%term	/' > \
	    ${.TARGET}

.depend: pcclocal.h pcctokens

.include <bsd.prog.mk>
