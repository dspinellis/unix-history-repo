#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
SCCSID = "@(#)Makefile	5.4 (Berkeley) %G%"
DESTDIR=
ED=	-ex
AS=	-as
RM=	rm -f
EYACC=	../eyacc/eyacc
CFLAGS=	-O -I. -I${SRCDIR} -w -DRMOTHERS
SRCDIR=	../src
BINDIR=	../pxp
#
# Files which are specific to PXP
#
LOCOBJ=	Version.o call.o case.o const.o cset.o error.o fdec.o func.o \
	lab.o lval.o main.o pmon.o pp.o proc.o rec.o rval.o stat.o \
	type.o var.o y.tab.o yycomm.o yyerror.o \
	yymain.o yyput.o rmothers.o
#
# Files which are shared with pi and pc0
#
SRCOBJ=	TRdata.o hash.o nl.o string.o subr.o tree.o treen.o yycopy.o \
	yycosts.o yyget.o yyid.o yylex.o yypanic.o yyparse.o yyprint.o \
	yyrecover.o yyseman.o yytree.o
SRCSRC=	TRdata.c hash.c nl.c string.c subr.c tree.c treen.c yycopy.c \
	yycosts.c yyget.c yyid.c yylex.c yypanic.c yyparse.c yyprint.c \
	yyrecover.c yyseman.c yytree.c

all: a.out

a.out:	${LOCOBJ} ${SRCOBJ}
	${CC} ${CFLAGS} ${LOCOBJ} ${SRCOBJ}

${SRCOBJ}:
	-ln ${SRCDIR}/$*.c $*.c
	${CC} ${CFLAGS} -c $*.c
	${RM} $*.c

y.tab.h y.tab.c: ${SRCDIR}/pas.y
	${EYACC} ${SRCDIR}/pas.y >/dev/null
	${ED} - y.tab.c < ${SRCDIR}/gram

newversion: version
	version > Version.c

version: version.c Version.o
	cc -o version version.c Version.o

opcode.h: ${SRCDIR}/OPnames.h ${SRCDIR}/opc.c
	${RM} opcode.h
	cc ${SRCDIR}/opc.c -o opc
	./opc >opcode.h
	${RM} opc

cleandir clean:
	-rm -f a.out core errs version *.bak
	-rm -f *.o x* y.tab.c y.tab.h opcode.h

install: a.out
	install -s a.out ${DESTDIR}/usr/bin/pxp
#	cp a.outNOID /usr/bin/pxp34

print:
	@pr Makefile READ_ME
	@ls -ls | pr
	@size a.out *.o | pr
#	@cxref *.c | pr
	@pr 0.h main.c ${SRCDIR}/pas.y
	@pr yy.h yy*.c
	@pr [a-ln-x]*.c
#	@pr putn.s treen.s yycopy.s

depend:
	ls *.c | sed 's;^\([a-zA-Z0-9.]*\)\.c;\1.o: &;' > .depend
	cd ${SRCDIR}; ls ${SRCSRC} | sed \
		-e 's;^\([a-zA-Z0-9.]*\)\.c;\1.o: ${SRCDIR}/&;' \
		>>${BINDIR}/.depend
	grep '^#[ 	]*include' *.h ${SRCDIR}/*.h | sed \
		-e '/<.*>/d' \
		-e 's;\(.*\):[^"]*"\([^"]*\)".*;\1: \2;' >>.depend
	cd ${SRCDIR}; grep '^#[ 	]*include' ${SRCSRC} | sed \
		-e '/<.*>/d' \
		-e 's;:[^"]*"\([^"]*\)".*;: ${SRCDIR}/\1;' \
		-e 's;^\([a-zA-Z0-9.]*\)\.c;\1.o;' \
		-e 's;: ${SRCDIR}/0.h;: 0.h;' \
		-e 's;: ${SRCDIR}/whoami.h;: whoami.h;' \
		-e 's;: ${SRCDIR}/opcode.h;: opcode.h;' >>${BINDIR}/.depend
	grep '^#[ 	]*include' *.c | sed \
		-e '/<.*>/d' \
		-e 's;:[^"]*"\([^"]*\)".*;: ${SRCDIR}/\1;' \
		-e 's;^\([a-zA-Z0-9.]*\)\.c;\1.o;' \
		-e 's;: ${SRCDIR}/0.h;: 0.h;' \
		-e 's;: ${SRCDIR}/whoami.h;: whoami.h;' \
		-e 's;: ${SRCDIR}/opcode.h;: opcode.h;' >>.depend
