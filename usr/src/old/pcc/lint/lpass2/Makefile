#	@(#)Makefile	5.5 (Berkeley) %G%

MIP=	${.CURDIR}/../../mip

PROG=	lintpass2
SRCS=	hash.c lpass2.c
CFLAGS+=-I${MIP} -I. -I${.CURDIR}/../lpass1
BINDIR=	/usr/libexec
.PATH:	${.CURDIR}/../lpass1 ${MIP}
CLEANFILES+=pcclocal.h
NOMAN=	noman

.include <bsd.prog.mk>

pcclocal.h: localdefs.h /usr/include/pcc.h
	cat ${.ALLSRC} | \
	   egrep '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | \
	   sed -e 's/PCC[A-Z]*_//' > ${.TARGET}

${OBJS} .depend: pcclocal.h
