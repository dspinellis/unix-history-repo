SCCSID = "@(#)Makefile 1.21 %G%"
INSTALLNAME = $(DESTDIR)/usr/lib/pc0
WHOAMI = pc
VERSION = 2.1

MKSTR = /usr/ucb/mkstr
EYACC = /usr/ucb/eyacc
RM = -rm -f
GET = sccs -d${SRCDIR} get

CFLAGS = -O -w
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
	stat.o string.o subr.o \
	tmps.o tree.o type.o var.o \
	TRdata.o \
	treen.o yycopy.o \
	y.tab.o \
	yycosts.o yyerror.o yyget.o yyid.o yylex.o yymain.o yyoptions.o \
	yypanic.o yyparse.o yyprint.o yyput.o yyrecover.o yyseman.o yytree.o

a.out: ${OBJS} config.c
	${CC} ${CFLAGS} ${LDFLAGS} ${OBJS} config.c

sources: whoami.h ${SRCS} ${HDRS} ${OTHERS}
	
${SRCS} ${HDRS} ${OTHERS}:
	${GET} $@

.c.o:
	${RM} ${TMPDIR}/$*.c
	${MKSTR} - ${ERRORSTRINGS} ${TMPDIR}/ $*.c
	cd ${TMPDIR}; ${CC} ${CFLAGS} -I.. -c $*.c;\
		rm -f ../$*.o; mv $*.o  ../$*.o
	${RM} ${TMPDIR}/$*.c

whoami.h:
	${GET} ${REL} piwhoami.h
	mv piwhoami.h whoami.h

y.tab.h: pas.y gram
	${RM} y.tab.h y.tab.c
	${EYACC} pas.y > /dev/null
	ex - y.tab.c <gram
	@echo that makes y.tab.h and y.tab.c

opcode.h: OPnames.h opc.c
	${RM} opcode.h
	cc opc.c -o opc
	opc >opcode.h
	${RM} opc

pTree.h:
	echo "/* this is not pTree.h */" > pTree.h

config.c: CONFIG.c makefile
	  sed "s?VERSION?${VERSION}?g" < CONFIG.c \
	| sed "s?LIBDIR?${LIBDIR}?g" \
	| sed "s?ERRORSTRINGS?${ERRORSTRINGS}?g" \
	| sed "s?HOWFILE?${HOWFILE}?g" > config.c

clean:
	${RM} *.o ${TMPDIR}/*.c
	${RM} whoami.h y.tab.h y.tab.c y.tab.out
	${RM} ${ERRORSTRINGS}
	${RM} config.c
	${RM} a.out core *.list *.bak
	${RM} opc pic tags

print:	sources
	@pr makefile READ_ME
	@ls -ls | pr
	@cc -o pic pic.c
	@pic | pr
	@rm pic
	@pr 0.h whoami.h main.c pas.y
	@pr OPnames.h opcode.h tree.h
	@pr [a-ln-x]*.c
	@pr yy.h yy*.c

install: a.out
	sccs edit CONFIG.c ; sccs delget CONFIG.c
	make a.out
	cp ${ERRORSTRINGS} ${LIBDIR}/${ERRORSTRINGS}
	cp ${INSTALLDIR}/pi ${INSTALLDIR}/pi.bak
	cp a.out ${INSTALLDIR}/pi

depend:	sources
	/bin/grep '^#[ 	]*include' *.h \
		| sed '/<.*>/d' \
		| sed 's/\(.*\):[^"]*"\([^"]*\)".*/\1: \2/' >makedep
	/bin/grep '^#[ 	]*include' *.c \
		| sed '/<.*>/d' \
		| sed 's/:[^"]*"\([^"]*\)".*/: \1/' \
		| sed 's/\.c/.o/' >>makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp makefile makefile.bak
	ed - makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> makefile
	echo '# see make depend above' >> makefile

# DO NOT DELETE THIS LINE -- make depend uses it

0.h: pTree.h
yy.h: y.tab.h
TRdata.o: whoami.h
TRdata.o: 0.h
ato.o: whoami.h
ato.o: 0.h
call.o: whoami.h
call.o: 0.h
call.o: tree.h
call.o: opcode.h
call.o: objfmt.h
call.o: pc.h
call.o: pcops.h
case.o: whoami.h
case.o: 0.h
case.o: tree.h
case.o: opcode.h
clas.o: whoami.h
clas.o: 0.h
clas.o: tree.h
const.o: whoami.h
const.o: 0.h
const.o: tree.h
conv.o: whoami.h
conv.o: 0.h
conv.o: opcode.h
conv.o: pcops.h
cset.o: whoami.h
cset.o: 0.h
cset.o: tree.h
cset.o: opcode.h
cset.o: objfmt.h
cset.o: pc.h
cset.o: pcops.h
error.o: whoami.h
error.o: 0.h
error.o: yy.h
fdec.o: whoami.h
fdec.o: 0.h
fdec.o: tree.h
fdec.o: opcode.h
fdec.o: objfmt.h
fdec.o: align.h
fdec.o: pc.h
fdec.o: pcops.h
fend.o: whoami.h
fend.o: 0.h
fend.o: tree.h
fend.o: opcode.h
fend.o: objfmt.h
fend.o: align.h
fend.o: pc.h
fend.o: pcops.h
fhdr.o: whoami.h
fhdr.o: 0.h
fhdr.o: tree.h
fhdr.o: opcode.h
fhdr.o: objfmt.h
fhdr.o: align.h
fhdr.o: pc.h
fhdr.o: pcops.h
flvalue.o: whoami.h
flvalue.o: 0.h
flvalue.o: tree.h
flvalue.o: opcode.h
flvalue.o: objfmt.h
flvalue.o: pc.h
flvalue.o: pcops.h
forop.o: whoami.h
forop.o: 0.h
forop.o: opcode.h
forop.o: tree.h
forop.o: objfmt.h
forop.o: pc.h
forop.o: pcops.h
flvalue.o: whoami.h
flvalue.o: 0.h
flvalue.o: tree.h
flvalue.o: opcode.h
flvalue.o: objfmt.h
flvalue.o: pc.h
flvalue.o: pcops.h
func.o: whoami.h
func.o: 0.h
func.o: tree.h
func.o: opcode.h
gen.o: whoami.h
gen.o: 0.h
gen.o: tree.h
gen.o: opcode.h
gen.o: objfmt.h
hash.o: whoami.h
hash.o: 0.h
hash.o: yy.h
lab.o: whoami.h
lab.o: 0.h
lab.o: tree.h
lab.o: opcode.h
lab.o: objfmt.h
lab.o: pc.h
lab.o: pcops.h
lookup.o: whoami.h
lookup.o: 0.h
lval.o: whoami.h
lval.o: 0.h
lval.o: tree.h
lval.o: opcode.h
lval.o: objfmt.h
lval.o: pc.h
lval.o: pcops.h
main.o: whoami.h
main.o: 0.h
main.o: yy.h
main.o: objfmt.h
main.o: config.h
nl.o: whoami.h
nl.o: 0.h
nl.o: opcode.h
nl.o: objfmt.h
opc.o: OPnames.h
pic.o: OPnames.h
proc.o: whoami.h
proc.o: 0.h
proc.o: tree.h
proc.o: opcode.h
proc.o: objfmt.h
put.o: whoami.h
put.o: opcode.h
put.o: 0.h
put.o: objfmt.h
put.o: pc.h
put.o: OPnames.h
rec.o: whoami.h
rec.o: 0.h
rec.o: tree.h
rec.o: opcode.h
rval.o: whoami.h
rval.o: 0.h
rval.o: tree.h
rval.o: opcode.h
rval.o: objfmt.h
rval.o: pc.h
rval.o: pcops.h
savenl.o: whoami.h
savenl.o: 0.h
savenl.o: objfmt.h
savenl.o: defs.h
savenl.o: object.h
savenl.o: object/objsym.rep
savenl.o: mappings.h
savenl.o: mappings/filetab.h
stat.o: whoami.h
stat.o: 0.h
stat.o: tree.h
stat.o: objfmt.h
stat.o: pcops.h
stat.o: pc.h
stat.o: opcode.h
stklval.o: whoami.h
stklval.o: 0.h
stklval.o: tree.h
stklval.o: opcode.h
stklval.o: objfmt.h
stkrval.o: whoami.h
stkrval.o: 0.h
stkrval.o: tree.h
stkrval.o: opcode.h
stkrval.o: objfmt.h
stkrval.o: pcops.h
string.o: whoami.h
string.o: 0.h
string.o: send.h
subr.o: whoami.h
subr.o: 0.h
tmps.o: whoami.h
tmps.o: 0.h
tmps.o: pc.h
tree.o: whoami.h
tree.o: 0.h
type.o: whoami.h
type.o: 0.h
type.o: tree.h
type.o: objfmt.h
var.o: whoami.h
var.o: 0.h
var.o: align.h
var.o: iorec.h
var.o: pc.h
var.o: pcops.h
y.tab.o: whoami.h
y.tab.o: 0.h
y.tab.o: yy.h
y.tab.o: tree.h
yycopy.o: 0.h
yycopy.o: yy.h
yycosts.o: whoami.h
yycosts.o: 0.h
yycosts.o: yy.h
yyerror.o: whoami.h
yyerror.o: 0.h
yyerror.o: yy.h
yyget.o: whoami.h
yyget.o: 0.h
yyget.o: yy.h
yyid.o: whoami.h
yyid.o: 0.h
yyid.o: yy.h
yylex.o: whoami.h
yylex.o: 0.h
yylex.o: yy.h
yymain.o: whoami.h
yymain.o: 0.h
yymain.o: yy.h
yymain.o: objfmt.h
yyoptions.o: whoami.h
yyoptions.o: 0.h
yyoptions.o: yy.h
yypanic.o: whoami.h
yypanic.o: 0.h
yypanic.o: yy.h
yyparse.o: whoami.h
yyparse.o: 0.h
yyparse.o: yy.h
yyprint.o: whoami.h
yyprint.o: 0.h
yyprint.o: yy.h
yyput.o: whoami.h
yyput.o: 0.h
yyput.o: tree.h
yyput.o: yy.h
yyrecover.o: whoami.h
yyrecover.o: 0.h
yyrecover.o: yy.h
yyseman.o: whoami.h
yyseman.o: 0.h
yyseman.o: yy.h
yytree.o: whoami.h
yytree.o: 0.h
yytree.o: tree.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
