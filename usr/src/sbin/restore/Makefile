#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
DESTDIR=
CFLAGS= -O
FILES=	main.c interactive.c restore.c dirs.c symtab.c tape.c utilities.c
OBJS=	main.o interactive.o restore.o dirs.o symtab.o utilities.o

all: restore rrestore

restore: ${OBJS} tape.o
	cc ${LDFLAGS} ${CFLAGS} -o restore ${OBJS} tape.o

rrestore: ${OBJS} rtape.o dumprmt.o
	cc ${LDFLAGS} -o rrestore ${OBJS} rtape.o dumprmt.o

rtape.o: tape.c
	cp tape.c rtape.c
	cc ${CFLAGS} -c -DRRESTORE rtape.c
	rm -f rtape.c

dumprmt.o: ../dump/dumprmt.c
	cc ${CFLAGS} -c ../dump/dumprmt.c

install: rrestore restore
	install restore ${DESTDIR}/etc/restore
	install -m 4755 rrestore ${DESTDIR}/etc/rrestore

clean:
	rm -f restore rrestore *.o errs a.out core

lint:
	lint ${FILES}

print:
	@pr /usr/include/dumprestor.h restore.h ${FILES}

${OBJS}: restore.h
