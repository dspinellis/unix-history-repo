# %W% (Berkeley) %G%
CFLAGS=	-O 

OBJS=	cerror.o mcount.o udiv.o urem.o

.c.o:
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

.s.o:
	${AS} -o $*.o $*.s
	-ld -x -r $*.o
	mv a.out $*.o

all: ${OBJS}

clean:
	rm -f ${OBJS}
