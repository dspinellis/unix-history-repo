#	@(#)Makefile	5.3 (Berkeley) 6/25/90

PROG=	f1
MIP=	${.CURDIR}/../mip
CCOM=	${.CURDIR}/../ccom.${MACHINE}
CFLAGS+=-DFORT -I. -I${.CURDIR} -I${CCOM} -I${MIP}
SRCS=	allo.c common.c fort.c local2.c match.c order.c reader.c table.c
NOMAN=	noman
.PATH:	${MIP} ${CCOM}
CLEANFILES+=pcclocal.h

table.o:
	${CC} ${CFLAGS} -c -R ${.IMPSRC}

common.o:
	${CC} ${CFLAGS} -DPASS2COMMON -c ${.IMPSRC}

pcclocal.h: localdefs.h /usr/include/pcc.h
	cat ${.ALLSRC} | \
	    egrep '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | \
	    sed -e 's/PCC[A-Z]*_//' > ${.TARGET}

.include <bsd.prog.mk>

${OBJS} .depend: pcclocal.h


