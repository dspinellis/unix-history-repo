#	@(#)Makefile	8.3 (Berkeley) %G%

PROG=	dbtest
OBJS=	dbtest.o #strerror.o
CFLAGS=	-O -DDEBUG -DSTATISTICS
LIB=	../PORT/bsd.44/libdb.a

dbtest: ${OBJS} ${LIB}
	${CC} -o $@ ${OBJS} ${LIB}

clean:
	rm -f ${OBJS} ${PROG} t1 t2 t3
