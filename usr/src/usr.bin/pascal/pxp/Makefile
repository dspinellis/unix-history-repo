SCCSID = "@(#)Makefile	1.6 (Berkeley) %G%"
DESTDIR=
ED=	-ex
AS=	-as
EYACC=	/usr/ucb/eyacc
CFLAGS=	-O -DPXP -w
OBJS=	call.o case.o const.o cset.o \
	error.o fdec.o func.o hash.o lab.o lval.o \
	main.o nl.o pmon.o pp.o proc.o rec.o rval.o \
	stat.o string.o subr.o tree.o type.o var.o y.tab.o \
	yycomm.o yycosts.o yyerror.o yyget.o yyid.o yylex.o yymain.o \
	yypanic.o yyparse.o yyprint.o yyput.o yyrecover.o \
	yyseman.o yytree.o
OBJS2=	doprnt.o printf.o treen.o yycopy.o TRdata.o Version.o

all: a.out

a.out:	${OBJS} ${OBJS2}
	${CC} ${CFLAGS} ${OBJS} ${OBJS2}

doprnt.o: doprnt.c
	${CC} ${CFLAGS} -E doprnt.c > doprnt.s
	${AS} -o doprnt.o doprnt.s
	-rm -f doprnt.s

y.tab.h y.tab.c: pas.y
	${EYACC} pas.y >/dev/null
	${ED} - y.tab.c < gram

version: version.c Version.o
	cc -o version version.c Version.o

newversion: version
	version > Version.c

clean:
	-rm -f pi1:2strings a.out core
	-rm -f *.o x* y.tab.c y.tab.h

install: a.out
	cp a.out ${DESTDIR}/usr/ucb/pxp
#	cp a.outNOID /usr/ucb/pxp34

print:
	@pr makefile READ_ME
	@ls -ls | pr
	@size a.out *.o | pr
#	@cxref *.c | pr
	@pr 0.h main.c pas.y
	@pr yy.h yy*.c
	@pr [a-ln-x]*.c
#	@pr putn.s treen.s yycopy.s

depend:	
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

# DO NOT DELETE THIS LINE
# DEPENDENCIES MUST END AT END OF FILE
yy.h: y.tab.h
TRdata.o: whoami.h
TRdata.o: 0.h
call.o: 0.h
call.o: tree.h
case.o: 0.h
case.o: tree.h
const.o: 0.h
const.o: tree.h
cset.o: 0.h
cset.o: tree.h
error.o: 0.h
error.o: yy.h
fdec.o: 0.h
fdec.o: tree.h
func.o: 0.h
func.o: tree.h
hash.o: whoami.h
hash.o: 0.h
hash.o: yy.h
lab.o: 0.h
lval.o: 0.h
lval.o: tree.h
main.o: 0.h
nl.o: 0.h
nl.o: opcode.h
pmon.o: 0.h
pp.o: 0.h
proc.o: 0.h
proc.o: tree.h
rec.o: 0.h
rval.o: 0.h
rval.o: tree.h
stat.o: 0.h
stat.o: tree.h
string.o: 0.h
subr.o: 0.h
tree.o: 0.h
type.o: 0.h
type.o: tree.h
var.o: 0.h
var.o: tree.h
y.tab.o: whoami.h
y.tab.o: 0.h
y.tab.o: yy.h
y.tab.o: tree.h
yycomm.o: 0.h
yycomm.o: yy.h
yycopy.o: 0.h
yycopy.o: yy.h
yycosts.o: whoami.h
yycosts.o: 0.h
yycosts.o: yy.h
yyerror.o: 0.h
yyerror.o: yy.h
yyget.o: 0.h
yyget.o: yy.h
yyid.o: 0.h
yyid.o: yy.h
yylex.o: 0.h
yylex.o: yy.h
yymain.o: 0.h
yymain.o: yy.h
yypanic.o: 0.h
yypanic.o: yy.h
yyparse.o: 0.h
yyparse.o: yy.h
yyprint.o: 0.h
yyprint.o: yy.h
yyput.o: 0.h
yyput.o: tree.h
yyput.o: yy.h
yyrecover.o: 0.h
yyrecover.o: yy.h
yyseman.o: 0.h
yyseman.o: yy.h
yytree.o: 0.h
yytree.o: tree.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
