#	@(#)Makefile	5.4 (Berkeley) 6/29/90

PROG=	pc0
SRCDIR=	${.CURDIR}/../src
CFLAGS=	-I. -I${.CURDIR} -I${SRCDIR}
.PATH:	${SRCDIR}
BINDIR=	/usr/libexec/pascal
NOMAN=	noman

EYACC=	/usr/libexec/pascal/eyacc

WHOAMI=	pc
.include "${SRCDIR}/config.include"

SRCS=	TRdata.c ato.c call.c case.c clas.c const.c conv.c cset.c error.c \
	fdec.c fend.c fhdr.c flvalue.c forop.c func.c gen.c hash.c lab.c \
	lookup.c lval.c main.c nl.c p2put.c pccaseop.c pcfunc.c pclval.c \
	pcproc.c proc.c put.c rec.c rval.c savenl.c sconv.c stab.c stat.c \
	stklval.c stkrval.c string.c subr.c tmps.c tree.c treen.c type.c \
	var.c yycopy.c yycosts.c yyerror.c yyget.c yyid.c yylex.c yymain.c \
	yyoptions.c yypanic.c yyparse.c yyprint.c yyput.c yyrecover.c \
	yyseman.c yytree.c
OBJS+=	config.c y.tab.o
CLEANFILES+=opc y.tab.h y.tab.c y.tab.out ${WHOAMI}*strings config.c \
	    opcode.h picture opc pic *.list ${ERRORSTRINGS} config.o

.c.o:
	rm -f ${.IMSRC:T}
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
	@echo make y.tab.h and y.tab.c
	rm -f y.tab.h y.tab.c
	${EYACC} ${SRCDIR}/pas.y > /dev/null
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
