# @(#)Makefile	4.6 12/17/82
OBJS=	gethostbyname.o gethostbyaddr.o gethostent.o \
	getnetbyaddr.o getnetent.o getnetbyname.o \
	getproto.o getprotoent.o getprotoname.o \
	getservent.o getservbyport.o getservbyname.o \
	raddr.o rcmd.o rexec.o rhost.o ruserpass.o
CFLAGS=	-O

.c.o:
	${CC} -p -c ${CFLAGS} $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

all:	${OBJS}

clean:
	rm -f ${OBJS} profiled/*.o errs a.out core
