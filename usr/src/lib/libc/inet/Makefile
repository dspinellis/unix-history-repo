# @(#)Makefile	4.2 (Berkeley) 6/27/83

OBJS=	inet_addr.o inet_network.o \
	inet_netof.o inet_ntoa.o inet_lnaof.o \
	inet_makeaddr.o
CFLAGS=	-O

.c.o:
	${CC} -p -c ${CFLAGS} $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

inetlib inetlib_p: ${OBJS}
	@echo "building profiled inetlib"
	@cd profiled; ar cru ../inetlib_p ${OBJS}
	@echo "building normal inetlib"
	@ar cru inetlib ${OBJS}

clean:
	rm -f inetlib inetlib_p ${OBJS} profiled/*.o errs a.out core
