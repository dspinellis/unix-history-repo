# @(#)Makefile	4.5 12/17/82
OBJS=	inet_addr.o inet_network.o \
	inet_netof.o inet_lnaof.o \
	inet_makeaddr.o
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
