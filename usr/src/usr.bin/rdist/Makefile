#	Makefile	4.9	85/06/01

RDIST = /usr/ucb/rdist
DESTDIR=
SRCS =	docmd.c expand.c gram.y lookup.c main.c server.c
OBJS =	docmd.o expand.o gram.o lookup.o main.o server.o
LINT =	lint -ps
CFLAGS= -O -DRDIST=\"${RDIST}\"

rdist:	${OBJS}
	${CC} -o rdist ${OBJS}

${OBJS}:  defs.h

clean:
	rm -f *.o gram.c errs

install: rdist
	install -s -m 4751 rdist ${DESTDIR}${RDIST}

lint:	docmd.c expand.c gram.c lookup.c main.c server.c
	${LINT} docmd.c expand.c gram.c lookup.c main.c server.c

print:	${SRCS}
	lpr -p ${SRCS} defs.h
