#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	f1
MIP=	${.CURDIR}/../mip
CCOM=	${.CURDIR}/../ccom.${MACHINE}
CFLAGS+=-DFORT -I. -I${.CURDIR} -I${CCOM} -I${MIP}
SRCS=	allo.c common.c fort.c local2.c match.c order.c reader.c table.c
NOMAN=	noman
.PATH:	${MIP} ${CCOM}
CLEANFILES+=pcclocal.h

table.o: ${CCOM}/table.c
	${CC} ${CFLAGS} -c -R ${CCOM}/table.c

common.o: ${MIP}/common.c
	${CC} ${CFLAGS} -DPASS2COMMON -c ${.ALLSRC}

pcclocal.h: ${.CURDIR}/../ccom.vax/localdefs.h /usr/include/pcc.h
	cat ${.ALLSRC} | \
	    egrep '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | \
	    sed -e 's/PCC[A-Z]*_//' > ${.TARGET}

.depend: pcclocal.h

.include <bsd.prog.mk>

