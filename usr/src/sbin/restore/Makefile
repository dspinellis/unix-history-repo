# Copyright (c) 1982 Regents of the University of California
#
#	@(#)Makefile	4.8	(Berkeley)	%G%
#
DESTDIR=
CFLAGS= -O
FILES=	main.c restore.c dirs.c symtab.c tape.c utilities.c
OBJS=	main.o restore.o dirs.o symtab.o utilities.o

all: restore rrestore

restore: ${OBJS} tape.o
	cc ${LDFLAGS} ${CFLAGS} -o restore ${OBJS} tape.o

rrestore: ${OBJS} rtape.o dumprmt.o
	cc ${LDFLAGS} -o rrestore ${OBJS} rtape.o dumprmt.o -ljobs

rtape.o: rtape.c
	cc ${CFLAGS} -c -DRRESTOR rtape.c

install: rrestore restore
	install restore ${DESTDIR}/etc/restore
	install -m 4755 rrestore ${DESTDIR}/etc/rrestore

clean:
	rm -f restore rrestore *.o errs

lint:
	lint ${FILES}

print:
	@pr /usr/include/dumprestor.h restore.h ${FILES}

${OBJS}: restore.h
