#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)pimakefile	5.2 (Berkeley) 9/7/85
#
SCCSID = "@(#)pimakefile 5.2 9/7/85"

INSTALLDIR=	${DESTDIR}/usr/bin
INSTALLNAME=	${INSTALLDIR}/pi
LIBDIR=	${DESTDIR}/usr/lib
BINDIR=	../pi
SRCDIR=	../src
VPATH=	../src
WHOAMI=	pi
VERSION=	3.1
DATE=	9/7/85
EYACC=	../eyacc/eyacc
CFLAGS=	-O
ERRORSTRINGS=	${WHOAMI}${VERSION}strings
OLDSTRINGS=	${WHOAMI}[0-9]*.[0-9]*strings
HOWFILE=	how_${WHOAMI}

SRCS=	ato.c call.c case.c clas.c const.c conv.c cset.c error.c fdec.c \
	fend.c fhdr.c flvalue.c forop.c func.c gen.c hash.c lab.c lookup.c \
	lval.c stklval.c main.c nl.c proc.c put.c rec.c rval.c stkrval.c \
	savenl.c stat.c string.c subr.c tmps.c tree.c type.c var.c TRdata.c \
	treen.c yycopy.c yycosts.c yyerror.c yyget.c yyid.c yylex.c yymain.c \
	yyoptions.c yypanic.c yyparse.c yyprint.c yyput.c yyrecover.c \
	yyseman.c yytree.c p2put.c stab.c pcproc.c pcfunc.c pccaseop.c \
	pclval.c sconv.c
OBJS=	ato.o call.o case.o clas.o const.o conv.o cset.o error.o fdec.o \
	fend.o fhdr.o flvalue.o forop.o func.o gen.o hash.o lab.o lookup.o \
	lval.o stklval.o main.o nl.o proc.o put.o rec.o rval.o stkrval.o \
	savenl.o stat.o string.o subr.o tmps.o tree.o type.o var.o TRdata.o \
	treen.o yycopy.o yycosts.o yyerror.o yyget.o yyid.o yylex.o yymain.o \
	yyoptions.o yypanic.o yyparse.o yyprint.o yyput.o yyrecover.o \
	yyseman.o yytree.o p2put.o stab.o pcproc.o pcfunc.o pccaseop.o \
	pclval.o sconv.o

all a.out: ${OBJS} y.tab.o config.c
	${CC} ${CFLAGS} ${OBJS} y.tab.o config.c

${OBJS}:
	rm -f `basename $@ .o`.c
	cd ${SRCDIR}; \
	    mkstr - ${BINDIR}/${ERRORSTRINGS} ${BINDIR}/ `basename $@ .o`.c
	${CC} ${CFLAGS} -I. -I${SRCDIR} -c `basename $@ .o`.c
	rm -f `basename $@ .o`.c

lint:
	cd ${SRCDIR}; lint -I. -I${BINDIR} ${SRCS} ${BINDIR}/*.c

y.tab.h y.tab.c: ${SRCDIR}/pas.y ${SRCDIR}/gram
	rm -f y.tab.h y.tab.c
	${EYACC} ${SRCDIR}/pas.y > /dev/null
	ex - y.tab.c <${SRCDIR}/gram
	@echo that makes y.tab.h and y.tab.c

y.tab.o: y.tab.c y.tab.h
	rm -f x.y.tab.c
	mkstr - ${ERRORSTRINGS} x. y.tab.c
	${CC} ${CFLAGS} -I${SRCDIR} -I. -c x.y.tab.c
	mv x.y.tab.o y.tab.o
	rm -f x.y.tab.c

picture: ${SRCDIR}/OPnames.h ${SRCDIR}/pic.c
	rm -f picture
	cc ${SRCDIR}/pic.c -o pic
	pic >picture
	rm -f pic

opcode.h: ${SRCDIR}/OPnames.h ${SRCDIR}/opc.c
	rm -f opcode.h
	cc ${SRCDIR}/opc.c -o opc
	opc >opcode.h
	rm -f opc

${SRCDIR}/0.h:
	touch -f ${SRCDIR}/0.h

config.c: ${SRCDIR}/CONFIG.c
	sed -e "s?VERSION?${VERSION}?g" \
	    -e "s?DATE?${DATE}?g" \
	    -e "s?INSTALLDIR?${INSTALLDIR}?g" \
	    -e "s?LIBDIR?${LIBDIR}?g" \
	    -e "s?ERRORSTRINGS?${ERRORSTRINGS}?g" \
	    -e "s?HOWFILE?${HOWFILE}?g" \
	    < ${SRCDIR}/CONFIG.c >config.c

cleandir clean:
	rm -f *.o *.c y.tab.h y.tab.c y.tab.out ${WHOAMI}*strings
	rm -f config.c opcode.h picture a.out core *.list *.bak errs
	rm -f opc pic tags

install: a.out
	rm -f ${LIBDIR}/${OLDSTRINGS}
	install -c -m 664 ${ERRORSTRINGS} ${LIBDIR}/${ERRORSTRINGS}
	install -s -m 775 a.out ${INSTALLNAME}

# Make depend must make clean first, since otherwise VPATH screws up
# which sources we 'make depend' from, if there are any .c's lying around.
depend: clean ${SRCS} opcode.h y.tab.h
	mkdep ${CFLAGS} -I. ${SRCS}
