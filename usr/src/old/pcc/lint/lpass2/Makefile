#	@(#)Makefile	5.2 (Berkeley) %G%

MIP=	${.CURDIR}/../../../libexec/pcc/mip
CCOM=	${.CURDIR}/../../../libexec/pcc/ccom.vax

PROG=	lintpass2
SRCS=	hash.c lpass2.c
CFLAGS+=-I${MIP} -I. -I${.CURDIR}/../lpass1
BINDIR=	/usr/libexec
.PATH:	${.CURDIR}/../lpass1
NOMAN=	noman

.include <bsd.prog.mk>

pcclocal.h: ${CCOM}/localdefs.h /usr/include/pcc.h
	cat ${.ALLSRC} | \
	   egrep '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | \
	   sed -e 's/PCC[A-Z]*_//' > ${.TARGET}

.depend: pcclocal.h
