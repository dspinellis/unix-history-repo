#	@(#)Makefile	8.2 (Berkeley) %G%

PROG=	pxp
SRCDIR=	${.CURDIR}/../src
.PATH:	${SRCDIR}
EYACC=	${.CURDIR}/../eyacc/obj/eyacc

CFLAGS+=-DRMOTHERS -w -I. -I${.CURDIR} -I${SRCDIR}
SRCS=	TRdata.c Version.c call.c case.c const.c cset.c error.c fdec.c \
	func.c hash.c lab.c lval.c main.c nl.c pmon.c pp.c proc.c rec.c \
	rmothers.c rval.c stat.c string.c subr.c tree.c treen.c type.c \
	var.c y.tab.c yycomm.c yycopy.c yycosts.c yyerror.c yyget.c yyid.c \
	yylex.c yymain.c yypanic.c yyparse.c yyprint.c yyput.c yyrecover.c \
	yyseman.c yytree.c
CLEANFILES=opc y.tab.c y.tab.h opcode.h version

y.tab.h: ${SRCDIR}/pas.y ${SRCDIR}/gram
	if [ -f ${EYACC} ]; then \
		echo make y.tab.h and y.tab.c; \
		rm -f y.tab.h y.tab.c; \
		${EYACC} ${SRCDIR}/pas.y > /dev/null; \
	else \
		echo copy y.tab.h and y.tab.c; \
		cp ${SRCDIR}/y.tab.c ${SRCDIR}/y.tab.h .; \
	fi
	ex - y.tab.c < ${SRCDIR}/gram

newversion: version
	./version > Version.c

version: version.c Version.o
	${CC} -o version ${.CURDIR}/version.c Version.o

opcode.h: opc ${SRCDIR}/OPnames.h
	./opc > ${.TARGET}

opc: ${SRCDIR}/opc.c
	${CC} ${SRCDIR}/opc.c -o ${.TARGET}

.include <bsd.prog.mk>

${OBJS} .depend: opcode.h y.tab.h
