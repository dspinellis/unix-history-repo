#	Makefile	4.1	83/09/07

DESTDIR=
SRCS =	main.c gram.y docmd.c expand.c server.c
OBJS =	main.o gram.o docmd.o expand.o server.o
LINT =	lint -ps
CFLAGS=

rdist:	${OBJS}
	${CC} -o rdist ${OBJS}

${OBJS}:  defs.h

clean:
	rm -f *.o gram.c errs

install:
	install -s rdist ${DESTDIR}/usr/ucb/rdist

lint:	main.c gram.c docmd.c expand.c server.c
	${LINT} main.c gram.c docmd.c expand.c server.c

print:	${SRCS}
	lpr -p ${SRCS} defs.h
