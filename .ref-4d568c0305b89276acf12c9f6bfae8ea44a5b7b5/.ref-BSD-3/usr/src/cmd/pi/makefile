DESTDIR=
CFLAGS = -O -w
LDFLAGS = -z

OBJS =	ato.o \
	call.o case.o clas.o const.o conv.o cset.o \
	error.o fdec.o func.o gen.o hash.o \
	lab.o lookup.o lval.o \
	main.o nl.o proc.o put.o \
	rec.o rval.o \
	stat.o string.o subr.o \
	tree.o type.o var.o \
	TRdata.o \
	treen.o putn.o yycopy.o \
	y.tab.o \
	yycosts.o yyerror.o yyget.o yyid.o yylex.o yymain.o yyoptions.o \
	yypanic.o yyparse.o yyprint.o yyput.o yyrecover.o yyseman.o yytree.o

a.out: ${OBJS} version
	./version > Version.c
	${CC} ${CFLAGS} ${LDFLAGS} ${OBJS} Version.c

a.obj: ${OBJS}
	./version > Version.c
	${CC} ${CFLAGS} ${LDFLAGS} ${OBJS} Version.c -o a.obj

pTreeOBJS = pCopy.o tCopy.o dCopy.o
pTreeDIR = /usr/ucb/lib

a.ptree: ${OBJS} ${pTreeOBJS}
	./version > Version.c
	${CC} ${CFLAGS} ${LDFLAGS} ${OBJS} Version.c  \
		${pTreeOBJS} ${pTreeDIR}/pAddr.o -o a.ptree

ppcOBJS = ppc.o

a.ppc: ${OBJS} ${pccOBJS}
	./version > Version.c
	${CC} ${CFLAGS} ${LDFLAGS} ${OBJS} Version.c ${pccOBJS} -o a.ppc

TMPDIR = tmp
MKSTR = /usr/ucb/mkstr
ERRORSTRINGS = pi1.2strings

.c.o:
	rm -f ${TMPDIR}/$*.c
	${MKSTR} - ${ERRORSTRINGS} ${TMPDIR}/ $*.c
	cd ${TMPDIR} ; \
	${CC} ${CFLAGS} -I.. -I${pTreeDIR} -c $*.c ; \
	mv $*.o  ../$*.o
	rm -f ${TMPDIR}/$*.c

version: version.c
	${CC} version.c -o version

RM = -rm

clean:
	@echo "don't rm y.tab.c until we have an eyacc"
	${RM} *.o ${TMPDIR}/*.c
	${RM} ${ERRORSTRINGS}
	${RM} version Version.c
	${RM} a.out a.obj a.ptree a.ppc core *.list

print:
	@pr makefile READ_ME
	@ls -ls | pr
	@pr picture
	@pr 0.h whoami main.c pas.y
	@pr OPnames.h opcode.h tree.h
	@pr pNodeBodies.h pTags.h pTree.h
	@pr ppc.h
	@pr [a-ln-x]*.c
	@pr yy.h yy*.c

install: a.out
	cp ${ERRORSTRINGS} ${DESTDIR}/usr/lib/${ERRORSTRINGS}
	cp a.out ${DESTDIR}/usr/ucb/pi
	strip ${DESTDIR}/usr/ucb/pi

0.h:		whoami
TRdata.o:	0.h
ato.o:		0.h
call.o:		0.h
case.o:		0.h
clas.o:		0.h
const.o:	0.h
conv.o:		0.h
cset.o:		0.h
dCopy.o:	0.h
error.o:	0.h
fdec.o:		0.h
func.o:		0.h
gen.o:		0.h
hash.o:		0.h
lab.o:		0.h
lookup.o:	0.h
lval.o:		0.h
main.o:		0.h
nl.o:		0.h
pCopy.o:	0.h
ppc.o:		0.h
proc.o:		0.h
put.o:		0.h
rec.o:		0.h
rval.o:		0.h
stat.o:		0.h
string.o:	0.h
subr.o:		0.h
tCopy.o:	0.h
tree.o:		0.h
type.o:		0.h
var.o:		0.h
y.tab.o:	0.h
yycopy.o:	0.h
yycosts.o:	0.h
yyerror.o:	0.h
yyget.o:	0.h
yyid.o:		0.h
yylex.o:	0.h
yymain.o:	0.h
yyoptions.o:	0.h
yypanic.o:	0.h
yyparse.o:	0.h
yyprint.o:	0.h
yyput.o:	0.h
yyrecover.o:	0.h
yyseman.o:	0.h
yytree.o:	0.h
