DESTDIR=
ED=	-ed
AS=	-as
VERSION= version
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
	${CC} ${CFLAGS} -i ${OBJS} ${OBJS2}

doprnt.o: doprnt.c
	${CC} ${CFLAGS} -E doprnt.c > doprnt.s
	${AS} -o doprnt.o doprnt.s
	-rm -f doprnt.s

#y.tab.c: pas.y
#	${EYACC} pas.y
#	${ED} - y.tab.c < gram

clean:
	-rm -f pi1:2strings a.out core
	-echo dont rm y.tab.c
	-rm -f *.o x*

install:
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
