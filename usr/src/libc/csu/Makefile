# %W% (Berkeley) %G%
CFLAGS=	-O 

OBJS=	crt0.o mcrt0.o

.s.o:
	${AS} -o $*.o $*.s
	-ld -x -r $*.o
	mv a.out $*.o

all: ${OBJS}

clean:
	rm -f ${OBJS}
