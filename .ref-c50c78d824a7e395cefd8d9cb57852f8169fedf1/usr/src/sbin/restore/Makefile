#
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.6 (Berkeley) %G%
#
VPATH=	../dump:
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	main.c interactive.c restore.c dirs.c symtab.c tape.c utilities.c \
	dumprmt.c
OBJS=	main.o interactive.o restore.o dirs.o symtab.o utilities.o tape.o
ROBJS=	main.o interactive.o restore.o dirs.o symtab.o utilities.o rtape.o \
	dumprmt.o
MAN=	restore.0 rrestore.0

all: restore rrestore

restore: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJS}

rrestore: ${ROBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${ROBJS}

rtape.o: ${LIBC} tape.c
	${CC} ${CFLAGS} -c -DRRESTORE tape.c -o rtape.o

clean:
	rm -f ${ROBJS} ${OBJS} core restore rrestore

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 restore ${DESTDIR}/etc/restore
	install -s -o root -g bin -m 4755 rrestore ${DESTDIR}/etc/rrestore
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat8

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
