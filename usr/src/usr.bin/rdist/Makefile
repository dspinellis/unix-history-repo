#	Makefile	4.6	83/11/30

DESTDIR=
SRCS =	docmd.c expand.c gram.y lookup.c main.c server.c
OBJS =	docmd.o expand.o gram.o lookup.o main.o server.o
LINT =	lint -ps
CFLAGS= -O

rdist:	${OBJS}
	${CC} -o rdist ${OBJS}

${OBJS}:  defs.h

clean:
	rm -f *.o gram.c errs

install: rdist
	install -s -m 4751 rdist ${DESTDIR}/usr/local/rdist

lint:	docmd.c expand.c gram.c lookup.c main.c server.c
	${LINT} docmd.c expand.c gram.c lookup.c main.c server.c

print:	${SRCS}
	lpr -p ${SRCS} defs.h
