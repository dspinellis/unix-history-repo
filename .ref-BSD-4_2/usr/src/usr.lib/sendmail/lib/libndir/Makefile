# @(#)Makefile	4.4 (Berkeley) 10/13/82

DESTDIR	=
CFLAGS=	-O -I.

OBJS=	closedir.o opendir.o readdir.o seekdir.o telldir.o

.c.o:
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

libndir: ${OBJS}
	ar cru libndir ${OBJS}
	ranlib libndir

install: libndir
	cp dir.h ${DESTDIR}/usr/include/dir.h
	cp libndir ${DESTDIR}/usr/lib/libndir.a
	ranlib ${DESTDIR}/usr/lib/libndir.a
	cp directory.3s /usr/man/man3/directory.3s

clean:
	rm -f libndir ${OBJS}
