.c.o:
	${MKSTR} - pi1:2strings x $*.c
	${CC} -c ${CFLAGS} x$*.c
	mv x$*.o $*.o
	rm x$*.c
AS=	-as
MKSTR=	/usr/ucb/mkstr
CFLAGS=	-O -DPI -DPI1
OBJS=	TRdata.o ato.o call.o case.o clas.o const.o conv.o cset.o \
	error.o fdec.o func.o gen.o lab.o lookup.o lval.o \
	main.o nl.o printf.o proc.o put.o putn.o rec.o receive.o \
	rval.o signal.o stat.o string.o subr.o tree.o type.o var.o  \
	yyerror.o yymain.o

a.out:	${OBJS}
	${CC} -n -f ${OBJS} ../pascal/fpnofetchi/fp.o

clean:
	rm pi1:2strings a.out core
	rm *.o x*
	-ln ../pi0/pi1:2strings
install:
	cp a.out /usr/lib/pi1
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
