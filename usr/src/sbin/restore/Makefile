# Copyright (c) 1982 Regents of the University of California
#
#	@(#)Makefile	4.2	(Berkeley)	%G%
#
NETSER=	/usr/src/ucb/netser

OBJS=	dumprmt.o ${NETSER}/rcmd.o
CFLAGS=

all: rrestor restor

restor: restor.c
	cc -o restor restor.c

rrestor: ${OBJS} rrestor.o
	cc -o rrestor ${OBJS} rrestor.o -ljobs

rrestor.o: restor.c
	cc -c -DRRESTOR restor.c
	mv restor.o rrestor.o

install: rrestor restor
	cp rrestor /etc
	cp restor /etc

clean:
	rm -f restor rrestor *.o errs
