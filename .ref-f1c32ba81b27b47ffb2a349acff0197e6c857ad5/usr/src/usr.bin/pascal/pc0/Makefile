#	@(#)Makefile	5.6 (Berkeley) %G%

PROG=	pc0
SRCDIR=	${.CURDIR}/../src
CFLAGS=	-I. -I${.CURDIR} -I${SRCDIR}
.PATH:	${SRCDIR}
BINDIR=	/usr/libexec/pascal
NOMAN=	noman
CLEANFILES+=main.c xmain.c

GET = sccs -d${SRCDIR} get

WHOAMI=	pc
.include "${SRCDIR}/config.include"

SRCDIR = /usr/src/cmd/pc0
INSTALLDIR = /usr/ucb
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

ERRORSTRINGS = pi2.0strings

	yypanic.c yyparse.c yyprint.c yyput.c yyrecover.c yyseman.c yytree.c

HDRS =	0.h OPnames.h align.h iorec.h objfmt.h send.h tree.h yy.h \
	pc.h pcops.h

whoami.h:
	${GET} ${REL} piwhoami.h
	mv piwhoami.h whoami.h

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

	cp ${INSTALLDIR}/pi ${INSTALLDIR}/pi.bak
	cp a.out ${INSTALLDIR}/pi

${OBJS} .depend: opcode.h y.tab.h
