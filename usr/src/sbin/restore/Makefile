# Copyright (c) 1982 Regents of the University of California
#
#	@(#)Makefile	4.1	(Berkeley)	%G%
#
NETSER=	/usr/src/ucb/netser

OBJS=	dumprmt.o ${NETSER}/rcmd.o
CFLAGS=

all: rrestor restor

rrestor: ${OBJS} rrestor.o
	cc -o rrestor ${OBJS} rrestor.o -ljobs

restor: ${OBJS} restor.o
	cc -o restor ${OBJS} restor.o -ljobs

rrestor.o: restor.c
	cc -c -DRRESTOR restor.c
	mv restor.o rrestor.o

restor.o: restor.c
	cc -c restor.c

install: rrestor restor
	cp rrestor /etc
	cp restor /etc

clean:
	rm -f restor rrestor *.o errs
