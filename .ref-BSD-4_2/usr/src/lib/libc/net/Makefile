# 	Makefile	4.9	83/06/30
#

OBJS=	gethostbyname.o gethostbyaddr.o gethostent.o \
	getnetbyaddr.o getnetent.o getnetbyname.o \
	getproto.o getprotoent.o getprotoname.o \
	getservent.o getservbyport.o getservbyname.o \
	rcmd.o rexec.o ruserpass.o
CFLAGS=	-O

.c.o:
	${CC} -p -c ${CFLAGS} $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

netlib netlib_p: ${OBJS}
	@echo "building profiled netlib"
	@cd profiled; ar cru ../netlib_p ${OBJS}
	@echo "building normal netlib"
	@ar cru netlib ${OBJS}

clean:
	rm -f ${OBJS} profiled/*.o errs a.out core netlib netlib_p
