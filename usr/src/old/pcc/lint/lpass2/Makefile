#	@(#)Makefile	5.1 (Berkeley) %G%

MIP=	${.CURDIR}/../../../libexec/mip
PCC=	${.CURDIR}/../../../libexec/pcc

PROG=	lintpass2
SRCS=	hash.c lpass2.c
CFLAGS+=-I${MIP} -I. -I${.CURDIR}/../lpass1
BINDIR=	/usr/libexec
.PATH:	${.CURDIR}/../lpass1
NOMAN=	noman

.include <bsd.prog.mk>

pcclocal.h: ${PCC}/localdefs.h /usr/include/pcc.h
	cat /usr/include/pcc.h ${PCC}/localdefs.h | \
	   egrep '^#[ 	]*(define[ 	][ 	]*PCC(F|T|TM|OM)?_|ifdef|ifndef|endif)' | \
	   sed -e 's/PCC[A-Z]*_//' > ${.TARGET}

.depend: pcclocal.h
