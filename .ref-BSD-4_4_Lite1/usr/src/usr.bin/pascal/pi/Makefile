#	@(#)Makefile	8.2 (Berkeley) 4/19/94

PROG=	pi
SRCDIR=	${.CURDIR}/../src
CFLAGS=	-I. -I${.CURDIR} -I${SRCDIR}
.PATH:	${SRCDIR}

EYACC=	${.CURDIR}/../eyacc/obj/eyacc

SRCS=	ato.c call.c case.c clas.c const.c conv.c cset.c error.c fdec.c \
	fend.c fhdr.c flvalue.c forop.c func.c gen.c hash.c lab.c lookup.c \
	lval.c stklval.c main.c nl.c proc.c put.c rec.c rval.c stkrval.c \
	savenl.c stat.c string.c subr.c tmps.c tree.c type.c var.c TRdata.c \
	treen.c yycopy.c yycosts.c yyerror.c yyget.c yyid.c yylex.c yymain.c \
	yyoptions.c yypanic.c yyparse.c yyprint.c yyput.c yyrecover.c \
	yyseman.c yytree.c p2put.c stab.c pcproc.c pcfunc.c pccaseop.c \
	pclval.c sconv.c
OBJS+=	config.c y.tab.o
CLEANFILES+=opc y.tab.h y.tab.c y.tab.out ${WHOAMI}*strings config.c \
	    opcode.h picture opc pic *.list ${ERRORSTRINGS} config.o

WHOAMI=	pi
.include "${SRCDIR}/config.include"

.c.o:
	rm -f ${.IMPSRC:T}
	ln -s ${.IMPSRC} .
	mkstr - ${ERRORSTRINGS} x ${.IMPSRC:T}
	${CC} ${CFLAGS} -c x${.IMPSRC:T} -o ${.TARGET}
	rm -f x${.IMPSRC:T} ${.IMPSRC:T}

y.tab.o: y.tab.c y.tab.h
	rm -f x.y.tab.c
	mkstr - ${ERRORSTRINGS} x. y.tab.c
	${CC} ${CFLAGS} -c x.y.tab.c
	mv x.y.tab.o y.tab.o
	rm -f x.y.tab.c

y.tab.c: y.tab.h

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

picture: pic ${SRCDIR}/OPnames.h
	./pic > picture

pic: pic.c
	${CC} ${.ALLSRC} -o ${.TARGET}

opcode.h: opc ${SRCDIR}/OPnames.h ${SRCDIR}/opc.c
	./opc > ${.TARGET}

opc: opc.c
	${CC} ${.ALLSRC} -o ${.TARGET}

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${ERRORSTRINGS} \
	    ${DESTDIR}${LIBDATA}/${ERRORSTRINGS}

.include <bsd.prog.mk>

${OBJS} .depend: opcode.h y.tab.h
