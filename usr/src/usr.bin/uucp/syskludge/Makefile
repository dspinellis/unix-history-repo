#	@(#)Makefile	4.1 (Berkeley) 1/1/83
#
CFLAGS=	-O
OBJS=	access.o chdir.o chmod.o chown.o creat.o link.o \
	open.o stat.o unlink.o syskludge.o
SRCS=	README INSTALL Makefile access.c chdir.c chmod.c \
	chown.c creat.c link.c open.c stat.c unlink.c \
	syskludge.c testsys.c

.c.o:
	/lib/cpp -E $*.c | ${AS} -o $*.o
	-ld -x -r $*.o
	mv a.out $*.o

syskludge.a: ${OBJS}
	ar uv syskludge.a ${OBJS}
	ranlib syskludge.a

clean:
	rm -f ${OBJS} testsys syskludge.a *.o

creat.o: creat.c
	${CC} -c ${CFLAGS} creat.c

syskludge.o: syskludge.c
	${CC} -c ${CFLAGS} syskludge.c

testsys: testsys.c uucpdefs.o syskludge.a
	${CC} ${CFLAGS} testsys.c uucpdefs.o syskludge.a -o testsys

uucpdefs.o: uucpdefs.c ../uucpdefs.h
	${CC} -c uucpdefs.c
