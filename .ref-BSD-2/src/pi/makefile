.c.o:
	${MKSTR} - pi1.2strings x $*.c
	${CC} -c ${CFLAGS} x$*.c
	mv x$*.o $*.o
	rm x$*.c
AS=	-as
ED=	-ed
VERSION= version
EYACC=	/usr/ucb/eyacc
MKSTR=	/usr/ucb/mkstr
CFLAGS=	-O -DPI -DPI01 -t1
OBJS=	ato.o call.o case.o clas.o const.o conv.o cset.o \
	error.o fdec.o func.o gen.o hash.o lab.o lookup.o lval.o \
	main.o nl.o printf.o proc.o put.o putn.o rec.o rval.o \
	stat.o string.o subr.o tree.o treen.o type.o var.o wait.o \
	y.tab.o yycopy.o yycosts.o yyerror.o yyget.o yyid.o yylex.o yymain.o \
	yyoptions.o yypanic.o yyparse.o yyprint.o yyput.o yyrecover.o \
	yyseman.o yytree.o

a.out:	${OBJS}
	-echo "${VERSION} > Version.c"
	${CC} -c Version.c
	${CC} -i -f ${OBJS} Version.o ../pascal/opcodes/TRdata.o ../pascal/fpterp/fp.o

y.tab.c: pas.y
	${EYACC} pas.y
	${ED} - y.tab.c < gram
clean:
	rm pi1.2strings a.out core
	-echo dont rm y.tab.c
	rm *.o x*
install:
	cp a.out /usr/ucb/pi
	cp pi1.2strings /usr/lib/pi1.2strings

print:
	@pr makefile READ_ME
	@ls -ls | pr
	@size a.out *.o | pr
	@cxref *.c | pr
	@pr ../pascal/opcodes/picture
	@pr 0.h main.c pas.y
	@pr yy.h yy*.c
	@pr [a-ln-x]*.c
	@pr putn.s treen.s yycopy.s
