#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3 (Berkeley) %G%
#
SCCSID = "@(#)Makefile	5.3 (Berkeley) %G%"
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

clean:
	-rm -f a.out core errs version *.bak
	-rm -f *.o x* y.tab.c y.tab.h opcode.h

install: a.out
	install -s a.out ${DESTDIR}/usr/ucb/pxp
#	cp a.outNOID /usr/ucb/pxp34

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
	ls *.c | sed 's;^\([a-zA-Z0-9.]*\)\.c;\1.o: &;' >makedep
	cd ${SRCDIR}; ls ${SRCSRC} | sed \
		-e 's;^\([a-zA-Z0-9.]*\)\.c;\1.o: ${SRCDIR}/&;' \
		>>${BINDIR}/makedep
	/bin/grep '^#[ 	]*include' *.h ${SRCDIR}/*.h | sed \
		-e '/<.*>/d' \
		-e 's;\(.*\):[^"]*"\([^"]*\)".*;\1: \2;' >>makedep
	cd ${SRCDIR}; /bin/grep '^#[ 	]*include' ${SRCSRC} | sed \
		-e '/<.*>/d' \
		-e 's;:[^"]*"\([^"]*\)".*;: ${SRCDIR}/\1;' \
		-e 's;^\([a-zA-Z0-9.]*\)\.c;\1.o;' \
		-e 's;: ${SRCDIR}/0.h;: 0.h;' \
		-e 's;: ${SRCDIR}/whoami.h;: whoami.h;' \
		-e 's;: ${SRCDIR}/opcode.h;: opcode.h;' >>${BINDIR}/makedep
	/bin/grep '^#[ 	]*include' *.c | sed \
		-e '/<.*>/d' \
		-e 's;:[^"]*"\([^"]*\)".*;: ${SRCDIR}/\1;' \
		-e 's;^\([a-zA-Z0-9.]*\)\.c;\1.o;' \
		-e 's;: ${SRCDIR}/0.h;: 0.h;' \
		-e 's;: ${SRCDIR}/whoami.h;: whoami.h;' \
		-e 's;: ${SRCDIR}/opcode.h;: opcode.h;' >>makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE
# DEPENDENCIES MUST END AT END OF FILE
Version.o: Version.c
call.o: call.c
case.o: case.c
const.o: const.c
cset.o: cset.c
error.o: error.c
fdec.o: fdec.c
func.o: func.c
lab.o: lab.c
lval.o: lval.c
main.o: main.c
pmon.o: pmon.c
pp.o: pp.c
proc.o: proc.c
rec.o: rec.c
rmothers.o: rmothers.c
rval.o: rval.c
stat.o: stat.c
type.o: type.c
var.o: var.c
version.o: version.c
y.tab.o: y.tab.c
yycomm.o: yycomm.c
yyerror.o: yyerror.c
yymain.o: yymain.c
yyput.o: yyput.c
TRdata.o: ../src/TRdata.c
hash.o: ../src/hash.c
nl.o: ../src/nl.c
string.o: ../src/string.c
subr.o: ../src/subr.c
tree.o: ../src/tree.c
treen.o: ../src/treen.c
yycopy.o: ../src/yycopy.c
yycosts.o: ../src/yycosts.c
yyget.o: ../src/yyget.c
yyid.o: ../src/yyid.c
yylex.o: ../src/yylex.c
yypanic.o: ../src/yypanic.c
yyparse.o: ../src/yyparse.c
yyprint.o: ../src/yyprint.c
yyrecover.o: ../src/yyrecover.c
yyseman.o: ../src/yyseman.c
yytree.o: ../src/yytree.c
../src/yy.h: y.tab.h
TRdata.o: whoami.h
TRdata.o: 0.h
hash.o: whoami.h
hash.o: 0.h
hash.o: ../src/tree_ty.h
hash.o: ../src/yy.h
nl.o: whoami.h
nl.o: 0.h
nl.o: opcode.h
nl.o: ../src/objfmt.h
string.o: whoami.h
string.o: 0.h
string.o: ../src/send.h
subr.o: whoami.h
subr.o: 0.h
tree.o: whoami.h
tree.o: 0.h
treen.o: 0.h
treen.o: ../src/tree_ty.h
yycopy.o: whoami.h
yycopy.o: 0.h
yycopy.o: ../src/tree_ty.h
yycopy.o: ../src/yy.h
yycosts.o: whoami.h
yycosts.o: 0.h
yycosts.o: ../src/tree_ty.h
yycosts.o: ../src/yy.h
yyget.o: whoami.h
yyget.o: 0.h
yyget.o: ../src/tree_ty.h
yyget.o: ../src/yy.h
yyid.o: whoami.h
yyid.o: 0.h
yyid.o: ../src/tree_ty.h
yyid.o: ../src/yy.h
yylex.o: whoami.h
yylex.o: 0.h
yylex.o: ../src/tree_ty.h
yylex.o: ../src/yy.h
yypanic.o: whoami.h
yypanic.o: 0.h
yypanic.o: ../src/tree_ty.h
yypanic.o: ../src/yy.h
yyparse.o: whoami.h
yyparse.o: 0.h
yyparse.o: ../src/tree_ty.h
yyparse.o: ../src/yy.h
yyprint.o: whoami.h
yyprint.o: 0.h
yyprint.o: ../src/tree_ty.h
yyprint.o: ../src/yy.h
yyrecover.o: whoami.h
yyrecover.o: 0.h
yyrecover.o: ../src/tree_ty.h
yyrecover.o: ../src/yy.h
yyseman.o: whoami.h
yyseman.o: 0.h
yyseman.o: ../src/tree_ty.h
yyseman.o: ../src/yy.h
yytree.o: whoami.h
yytree.o: 0.h
yytree.o: ../src/tree.h
yytree.o: ../src/tree_ty.h
call.o: 0.h
call.o: ../src/tree.h
case.o: 0.h
case.o: ../src/tree.h
const.o: 0.h
const.o: ../src/tree.h
cset.o: 0.h
cset.o: ../src/tree.h
error.o: whoami.h
error.o: 0.h
error.o: ../src/yy.h
fdec.o: 0.h
fdec.o: ../src/tree.h
func.o: 0.h
func.o: ../src/tree.h
lab.o: 0.h
lval.o: 0.h
lval.o: ../src/tree.h
main.o: 0.h
pmon.o: 0.h
pp.o: 0.h
proc.o: 0.h
proc.o: ../src/tree.h
rec.o: 0.h
rmothers.o: 0.h
rmothers.o: ../src/tree.h
rval.o: 0.h
rval.o: ../src/tree.h
stat.o: 0.h
stat.o: ../src/tree.h
type.o: 0.h
type.o: ../src/tree.h
var.o: 0.h
var.o: ../src/tree.h
y.tab.o: whoami.h
y.tab.o: 0.h
y.tab.o: ../src/tree_ty.h
y.tab.o: ../src/yy.h
y.tab.o: ../src/tree.h
yycomm.o: whoami.h
yycomm.o: 0.h
yycomm.o: ../src/yy.h
yyerror.o: whoami.h
yyerror.o: 0.h
yyerror.o: ../src/yy.h
yymain.o: whoami.h
yymain.o: 0.h
yymain.o: ../src/yy.h
yyput.o: whoami.h
yyput.o: 0.h
yyput.o: ../src/tree.h
yyput.o: ../src/yy.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
