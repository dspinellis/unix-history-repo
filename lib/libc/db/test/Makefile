#	@(#)Makefile	8.5 (Berkeley) 9/7/93

PROG=	dbtest
OBJS1=	dbtest.o #strerror.o
#OBJS2=	rec_close.o rec_delete.o rec_get.o rec_open.o rec_put.o \
#	rec_search.o rec_seq.o rec_utils.o
#VPATH=	../recno
OBJS=	${OBJS1} ${OBJS2} ${OBJS3}

PDIR=	../PORT/bsd.44
CFLAGS=	-O -DDEBUG -DSTATISTICS -I${PDIR}/include -I${PDIR} #-I../recno
LIB=	${PDIR}/libdb.a

dbtest: ${OBJS} ${LIB}
	${CC} -o $@ ${OBJS} ${LIB} ${LIB}

clean:
	rm -f gmon.out ${OBJS} ${PROG} t1 t2 t3
