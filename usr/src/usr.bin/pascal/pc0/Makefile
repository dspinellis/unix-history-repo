#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3 (Berkeley) %G%
#
SCCSID = "@(#)Makefile 5.3 %G%"

INSTALLDIR = $(DESTDIR)/usr/lib
INSTALLNAME = ${INSTALLDIR}/pc0
BINDIR = ../pc0
SRCDIR = ../src
VPATH=	../src
WHOAMI = pc
VERSION = 3.1
DATE = %G%

MKSTR = /usr/ucb/mkstr
EYACC = ../eyacc/eyacc
RM = -rm -f
GET = sccs -d${SRCDIR} get

CFLAGS = -O
LDFLAGS = -z

SRCDIR = /usr/src/cmd/pc0
INSTALLDIR = /usr/ucb
LIBDIR = ${DESTDIR}/usr/lib
TMPDIR = tmp

ERRORSTRINGS = pi2.0strings

SRCS =	ato.c \
	call.c case.c clas.c const.c conv.c cset.c \
	error.c fdec.c flvalue.c func.c gen.c hash.c \
	lab.c lookup.c lval.c stklval.c \
	main.c nl.c proc.c put.c \
	rec.c rval.c stkrval.c\
	savenl.c \
	stat.c string.c subr.c \
	tmps.c tree.c type.c var.c \
	TRdata.c \
	treen.c yycopy.c \
	yycosts.c yyerror.c yyget.c yyid.c yylex.c yymain.c yyoptions.c \
	yypanic.c yyparse.c yyprint.c yyput.c yyrecover.c yyseman.c yytree.c

HDRS =	0.h OPnames.h align.h iorec.h objfmt.h send.h tree.h yy.h \
	pc.h pcops.h

OTHERS = pas.y opc.c CONFIG.c gram pic.c

OBJS =	ato.o \
	call.o case.o clas.o const.o conv.o cset.o \
	error.o fdec.o flvalue.o func.o gen.o hash.o \
	lab.o lookup.o lval.o stklval.o \
	main.o nl.o proc.o put.o \
	rec.o rval.o stkrval.o\
	savenl.o \
	stat.o string.o subr.o \
	tmps.o tree.o type.o var.o \
	TRdata.o \
	treen.o yycopy.o \
	yycosts.o yyerror.o yyget.o yyid.o yylex.o yymain.o yyoptions.o \
	yypanic.o yyparse.o yyprint.o yyput.o yyrecover.o yyseman.o yytree.o

a.out: ${OBJS} y.tab.o config.c
	${CC} ${CFLAGS} ${LDFLAGS} ${OBJS} y.tab.o config.c

${OBJS}:
	${RM} `basename $@ .o`.c
	cd ${SRCDIR}; \
	    ${MKSTR} - ${BINDIR}/${ERRORSTRINGS} ${BINDIR}/ `basename $@ .o`.c
	${CC} ${CFLAGS} -I. -I${SRCDIR} -c `basename $@ .o`.c
	${RM} `basename $@ .o`.c

lint:
	cd ${SRCDIR}; lint -I. -I${BINDIR} ${SRCS}  ${BINDIR}/*.c

whoami.h:
	${GET} ${REL} piwhoami.h
	mv piwhoami.h whoami.h

y.tab.h: ${SRCDIR}/pas.y ${SRCDIR}/gram
	${RM} y.tab.h y.tab.c
	${EYACC} ${SRCDIR}/pas.y > /dev/null
	ex - y.tab.c <${SRCDIR}/gram
	@echo that makes y.tab.h and y.tab.c
y.tab.o: y.tab.c y.tab.h
	${RM} x.y.tab.c
	${MKSTR} - ${ERRORSTRINGS} x. y.tab.c
	${CC} ${CFLAGS} -I${SRCDIR} -I. -c x.y.tab.c
	mv x.y.tab.o y.tab.o
	${RM} x.y.tab.c

picture: ${SRCDIR}/OPnames.h ${SRCDIR}/pic.c
	${RM} picture
	cc ${SRCDIR}/pic.c -o pic
	pic >picture
	${RM} pic

opcode.h: ${SRCDIR}/OPnames.h ${SRCDIR}/opc.c
	${RM} opcode.h
	cc ${SRCDIR}/opc.c -o opc
	opc >opcode.h
	${RM} opc

${SRCDIR}/0.h:
	${TOUCH} ${SRCDIR}/0.h

config.c: ${SRCDIR}/CONFIG.c
	sed -e "s?VERSION?${VERSION}?g" \
	    -e "s?DATE?${DATE}?g" \
	    -e "s?INSTALLDIR?${INSTALLDIR}?g" \
	    -e "s?LIBDIR?${LIBDIR}?g" \
	    -e "s?ERRORSTRINGS?${ERRORSTRINGS}?g" \
	    -e "s?HOWFILE?${HOWFILE}?g" \
	    < ${SRCDIR}/CONFIG.c >config.c

clean:
	${RM} *.o *.c
	${RM} whoami.h y.tab.h y.tab.c y.tab.out
	${RM} ${WHOAMI}*strings
	${RM} config.c opcode.h picture
	${RM} a.out core *.list *.bak errs
	${RM} opc pic tags

install: a.out
	${RM} ${LIBDIR}/${OLDSTRINGS}
	cp ${INSTALLDIR}/pi ${INSTALLDIR}/pi.bak
	cp a.out ${INSTALLDIR}/pi

depend: ${SRCS} opcode.h y.tab.h
	mkdep ${CFLAGS} -I. ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

ato.o: ../src/ato.c whoami.h ../src/0.h /usr/include/stdio.h
ato.o: /usr/include/sys/types.h
call.o: ../src/call.c whoami.h ../src/0.h /usr/include/stdio.h
call.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/objfmt.h
call.o: ../src/align.h ../src/pc.h /usr/include/setjmp.h /usr/include/pcc.h
call.o: ../src/tmps.h ../src/tree_ty.h
case.o: ../src/case.c whoami.h ../src/0.h /usr/include/stdio.h
case.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/tree_ty.h
clas.o: ../src/clas.c whoami.h ../src/0.h /usr/include/stdio.h
clas.o: /usr/include/sys/types.h ../src/tree.h ../src/tree_ty.h
const.o: ../src/const.c whoami.h ../src/0.h /usr/include/stdio.h
const.o: /usr/include/sys/types.h ../src/tree.h ../src/tree_ty.h
conv.o: ../src/conv.c whoami.h ../src/0.h /usr/include/stdio.h
conv.o: /usr/include/sys/types.h opcode.h /usr/include/pcc.h ../src/tree_ty.h
cset.o: ../src/cset.c whoami.h ../src/0.h /usr/include/stdio.h
cset.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/objfmt.h
cset.o: ../src/tree_ty.h ../src/pc.h /usr/include/setjmp.h /usr/include/pcc.h
cset.o: ../src/align.h
error.o: ../src/error.c whoami.h ../src/0.h /usr/include/stdio.h
error.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
fdec.o: ../src/fdec.c whoami.h ../src/0.h /usr/include/stdio.h
fdec.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/objfmt.h
fdec.o: ../src/align.h ../src/tmps.h ../src/pc.h /usr/include/setjmp.h
fdec.o: /usr/include/pcc.h
fend.o: ../src/fend.c whoami.h ../src/0.h /usr/include/stdio.h
fend.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/objfmt.h
fend.o: ../src/align.h ../src/tmps.h ../src/pc.h /usr/include/setjmp.h
fend.o: /usr/include/pcc.h ../src/tree_ty.h
fhdr.o: ../src/fhdr.c whoami.h ../src/0.h /usr/include/stdio.h
fhdr.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/objfmt.h
fhdr.o: ../src/align.h ../src/tree_ty.h ../src/pc.h /usr/include/setjmp.h
flvalue.o: ../src/flvalue.c whoami.h ../src/0.h /usr/include/stdio.h
flvalue.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/objfmt.h
flvalue.o: ../src/tree_ty.h ../src/pc.h /usr/include/setjmp.h
flvalue.o: /usr/include/pcc.h ../src/tmps.h
forop.o: ../src/forop.c whoami.h ../src/0.h /usr/include/stdio.h
forop.o: /usr/include/sys/types.h opcode.h ../src/tree.h ../src/objfmt.h
forop.o: ../src/pc.h /usr/include/setjmp.h /usr/include/pcc.h ../src/tmps.h
forop.o: ../src/tree_ty.h
flvalue.o: whoami.h
flvalue.o: 0.h
flvalue.o: tree.h
flvalue.o: opcode.h
flvalue.o: objfmt.h
flvalue.o: pc.h
flvalue.o: pcops.h
func.o: ../src/func.c whoami.h
gen.o: ../src/gen.c whoami.h
hash.o: ../src/hash.c whoami.h ../src/0.h /usr/include/stdio.h
hash.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
lab.o: ../src/lab.c whoami.h ../src/0.h /usr/include/stdio.h
lab.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/objfmt.h
lab.o: ../src/pc.h /usr/include/setjmp.h /usr/include/pcc.h ../src/tree_ty.h
lookup.o: ../src/lookup.c whoami.h ../src/0.h /usr/include/stdio.h
lookup.o: /usr/include/sys/types.h
lval.o: ../src/lval.c whoami.h ../src/0.h /usr/include/stdio.h
lval.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/objfmt.h
lval.o: ../src/tree_ty.h ../src/pc.h /usr/include/setjmp.h /usr/include/pcc.h
stklval.o: ../src/stklval.c whoami.h ../src/0.h /usr/include/stdio.h
stklval.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/objfmt.h
stklval.o: ../src/tree_ty.h
main.o: ../src/main.c whoami.h ../src/0.h /usr/include/stdio.h
main.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
main.o: /usr/include/signal.h /usr/include/machine/trap.h ../src/objfmt.h
main.o: ../src/config.h
nl.o: ../src/nl.c whoami.h ../src/0.h /usr/include/stdio.h
nl.o: /usr/include/sys/types.h opcode.h ../src/objfmt.h
proc.o: ../src/proc.c whoami.h
put.o: ../src/put.c whoami.h opcode.h ../src/0.h /usr/include/stdio.h
put.o: /usr/include/sys/types.h ../src/objfmt.h ../src/pc.h
put.o: /usr/include/setjmp.h ../src/align.h ../src/OPnames.h
rec.o: ../src/rec.c whoami.h ../src/0.h /usr/include/stdio.h
rec.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/align.h
rec.o: ../src/tree_ty.h
rval.o: ../src/rval.c whoami.h ../src/0.h /usr/include/stdio.h
rval.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/objfmt.h
rval.o: ../src/pc.h /usr/include/setjmp.h /usr/include/pcc.h ../src/tmps.h
rval.o: ../src/tree_ty.h
stkrval.o: ../src/stkrval.c whoami.h ../src/0.h /usr/include/stdio.h
stkrval.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/objfmt.h
stkrval.o: ../src/align.h /usr/include/pcc.h ../src/tree_ty.h
savenl.o: ../src/savenl.c whoami.h
stat.o: ../src/stat.c whoami.h ../src/0.h /usr/include/stdio.h
stat.o: /usr/include/sys/types.h ../src/tree.h ../src/objfmt.h
stat.o: /usr/include/pcc.h ../src/pc.h /usr/include/setjmp.h ../src/tmps.h
stat.o: opcode.h ../src/tree_ty.h
string.o: ../src/string.c whoami.h ../src/align.h ../src/0.h
string.o: /usr/include/stdio.h /usr/include/sys/types.h
subr.o: ../src/subr.c whoami.h ../src/0.h /usr/include/stdio.h
subr.o: /usr/include/sys/types.h /usr/include/sys/stat.h
tmps.o: ../src/tmps.c whoami.h ../src/0.h /usr/include/stdio.h
tmps.o: /usr/include/sys/types.h ../src/objfmt.h ../src/pc.h
tmps.o: /usr/include/setjmp.h ../src/align.h ../src/tmps.h
tree.o: ../src/tree.c whoami.h ../src/0.h /usr/include/stdio.h
tree.o: /usr/include/sys/types.h
type.o: ../src/type.c whoami.h ../src/0.h /usr/include/stdio.h
type.o: /usr/include/sys/types.h ../src/tree.h ../src/objfmt.h ../src/tree_ty.h
var.o: ../src/var.c whoami.h ../src/0.h /usr/include/stdio.h
var.o: /usr/include/sys/types.h ../src/objfmt.h ../src/align.h ../src/iorec.h
var.o: /usr/include/stdio.h ../src/pc.h /usr/include/setjmp.h ../src/tmps.h
var.o: ../src/tree_ty.h
TRdata.o: ../src/TRdata.c whoami.h ../src/0.h /usr/include/stdio.h
TRdata.o: /usr/include/sys/types.h
treen.o: ../src/treen.c ../src/0.h /usr/include/stdio.h
treen.o: /usr/include/sys/types.h ../src/tree_ty.h
yycopy.o: ../src/yycopy.c whoami.h ../src/0.h /usr/include/stdio.h
yycopy.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yycosts.o: ../src/yycosts.c whoami.h ../src/0.h /usr/include/stdio.h
yycosts.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yyerror.o: ../src/yyerror.c whoami.h ../src/0.h /usr/include/stdio.h
yyerror.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yyget.o: ../src/yyget.c whoami.h ../src/0.h /usr/include/stdio.h
yyget.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yyid.o: ../src/yyid.c whoami.h ../src/0.h /usr/include/stdio.h
yyid.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yylex.o: ../src/yylex.c whoami.h ../src/0.h /usr/include/stdio.h
yylex.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yymain.o: ../src/yymain.c whoami.h ../src/0.h /usr/include/stdio.h
yymain.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yymain.o: /usr/include/a.out.h /usr/include/sys/exec.h ../src/objfmt.h
yymain.o: /usr/include/signal.h /usr/include/machine/trap.h ../src/config.h
yyoptions.o: ../src/yyoptions.c whoami.h ../src/0.h /usr/include/stdio.h
yyoptions.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yypanic.o: ../src/yypanic.c whoami.h ../src/0.h /usr/include/stdio.h
yypanic.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yyparse.o: ../src/yyparse.c whoami.h ../src/0.h /usr/include/stdio.h
yyparse.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yyprint.o: ../src/yyprint.c whoami.h ../src/0.h /usr/include/stdio.h
yyprint.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yyput.o: ../src/yyput.c whoami.h ../src/0.h /usr/include/stdio.h
yyput.o: /usr/include/sys/types.h ../src/tree.h ../src/tree_ty.h ../src/yy.h
yyput.o: y.tab.h
yyrecover.o: ../src/yyrecover.c whoami.h ../src/0.h /usr/include/stdio.h
yyrecover.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yyseman.o: ../src/yyseman.c whoami.h ../src/0.h /usr/include/stdio.h
yyseman.o: /usr/include/sys/types.h ../src/tree_ty.h ../src/yy.h y.tab.h
yytree.o: ../src/yytree.c whoami.h ../src/0.h /usr/include/stdio.h
yytree.o: /usr/include/sys/types.h ../src/tree.h ../src/tree_ty.h
p2put.o: ../src/p2put.c whoami.h ../src/0.h /usr/include/stdio.h
p2put.o: /usr/include/sys/types.h ../src/objfmt.h /usr/include/pcc.h
p2put.o: ../src/pc.h /usr/include/setjmp.h ../src/align.h ../src/tmps.h
stab.o: ../src/stab.c whoami.h ../src/0.h /usr/include/stdio.h
stab.o: /usr/include/sys/types.h ../src/objfmt.h ../src/yy.h y.tab.h
stab.o: /usr/include/stab.h ../src/pstab.h ../src/pc.h /usr/include/setjmp.h
pcproc.o: ../src/pcproc.c whoami.h ../src/0.h /usr/include/stdio.h
pcproc.o: /usr/include/sys/types.h ../src/tree.h ../src/objfmt.h opcode.h
pcproc.o: ../src/pc.h /usr/include/setjmp.h /usr/include/pcc.h ../src/tmps.h
pcproc.o: ../src/tree_ty.h
pcfunc.o: ../src/pcfunc.c whoami.h ../src/0.h /usr/include/stdio.h
pcfunc.o: /usr/include/sys/types.h ../src/tree.h ../src/objfmt.h opcode.h
pcfunc.o: ../src/pc.h /usr/include/setjmp.h /usr/include/pcc.h ../src/tmps.h
pcfunc.o: ../src/tree_ty.h
pccaseop.o: ../src/pccaseop.c whoami.h ../src/0.h /usr/include/stdio.h
pccaseop.o: /usr/include/sys/types.h ../src/tree.h ../src/objfmt.h
pccaseop.o: /usr/include/pcc.h ../src/pc.h /usr/include/setjmp.h ../src/tmps.h
pccaseop.o: ../src/tree_ty.h
pclval.o: ../src/pclval.c whoami.h ../src/0.h /usr/include/stdio.h
pclval.o: /usr/include/sys/types.h ../src/tree.h opcode.h ../src/objfmt.h
pclval.o: ../src/tree_ty.h ../src/pc.h /usr/include/setjmp.h /usr/include/pcc.h
sconv.o: ../src/sconv.c whoami.h ../src/0.h /usr/include/stdio.h
sconv.o: /usr/include/sys/types.h /usr/include/pcc.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
