.c.o:
	${MKSTR} - pi1:2strings x $*.c
	${CC} -c ${CFLAGS} x$*.c
	mv x$*.o $*.o
	rm x$*.c
ED=	-ed
AS=	-as
VERSION= version
EYACC=	/usr/ucb/eyacc
MKSTR=	/usr/ucb/mkstr
CFLAGS=	-O -DPI -DPI0
OBJS=	TRdata.o ato.o clas.o const.o conv.o \
	error.o fdec.o hash.o lab.o lookup.o \
	main.o nl.o printf.o rec.o send.o \
	string.o subr.o tree.o treen.o type.o var.o wait.o \
	y.tab.o yycopy.o yycosts.o yyerror.o yyget.o yyid.o yylex.o yymain.o \
	yyoptions.o yypanic.o yyparse.o yyprint.o yyput.o yyrecover.o \
	yyseman.o yytree.o

a.out:	${OBJS}
	-echo "${VERSION} > Version.c"
	${CC} -c Version.c
	${CC} -n -f ${OBJS} Version.o ../pascal/fpdata/fp.o ../pascal/fpdata/putc.o ../pascal/fpdata/perror.o
	-rm -f ../pi1/pi1:2strings
	ln pi1:2strings ../pi1/pi1:2strings

y.tab.c: pas.y
	${EYACC} pas.y
	${ED} - y.tab.c < gram
clean:
	rm pi1:2strings a.out core
	-echo dont rm y.tab.c
	rm *.o x*
install:
	cp a.out /usr/ucb/pi34
	cp pi1:2strings /usr/lib/pi1:2strings
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
