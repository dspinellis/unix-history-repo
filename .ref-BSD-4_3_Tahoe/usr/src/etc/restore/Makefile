#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5	(Berkeley)	6/2/87
#
VPATH=	../dump:
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	main.c interactive.c restore.c dirs.c symtab.c tape.c utilities.c \
	../dump/dumprmt.c
OBJS=	main.o interactive.o restore.o dirs.o symtab.o utilities.o tape.o
ROBJS=	main.o interactive.o restore.o dirs.o symtab.o utilities.o rtape.o \
	dumprmt.o

all: restore rrestore

restore: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJS}

rrestore: ${ROBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${ROBJS}

rtape.o: ${LIBC} tape.c
	${CC} ${CFLAGS} -c -DRRESTORE tape.c -o rtape.o

clean: FRC
	rm -f ${ROBJS} ${OBJS} core restore rrestore

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 restore ${DESTDIR}/etc/restore
	install -s -o root -g bin -m 4755 rrestore ${DESTDIR}/etc/rrestore

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

main.o: main.c restore.h /usr/include/stdio.h /usr/include/sys/param.h
main.o: /usr/include/sys/types.h /usr/include/signal.h
main.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
main.o: /usr/include/machine/endian.h /usr/include/sys/inode.h
main.o: /usr/include/sys/fs.h /usr/include/sys/dir.h
main.o: /usr/include/protocols/dumprestore.h /usr/include/signal.h
interactive.o: interactive.c restore.h /usr/include/stdio.h
interactive.o: /usr/include/sys/param.h /usr/include/sys/types.h
interactive.o: /usr/include/signal.h /usr/include/machine/trap.h
interactive.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
interactive.o: /usr/include/sys/inode.h /usr/include/sys/fs.h
interactive.o: /usr/include/sys/dir.h /usr/include/protocols/dumprestore.h
interactive.o: /usr/include/setjmp.h
restore.o: restore.c restore.h /usr/include/stdio.h /usr/include/sys/param.h
restore.o: /usr/include/sys/types.h /usr/include/signal.h
restore.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
restore.o: /usr/include/machine/endian.h /usr/include/sys/inode.h
restore.o: /usr/include/sys/fs.h /usr/include/sys/dir.h
dirs.o: dirs.c restore.h /usr/include/stdio.h /usr/include/sys/param.h
dirs.o: /usr/include/sys/types.h /usr/include/signal.h
dirs.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
dirs.o: /usr/include/machine/endian.h /usr/include/sys/inode.h
dirs.o: /usr/include/sys/fs.h /usr/include/sys/dir.h
dirs.o: /usr/include/protocols/dumprestore.h /usr/include/sys/file.h
symtab.o: symtab.c restore.h /usr/include/stdio.h /usr/include/sys/param.h
symtab.o: /usr/include/sys/types.h /usr/include/signal.h
symtab.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
symtab.o: /usr/include/machine/endian.h /usr/include/sys/inode.h
symtab.o: /usr/include/sys/fs.h /usr/include/sys/dir.h /usr/include/sys/stat.h
tape.o: tape.c restore.h /usr/include/stdio.h /usr/include/sys/param.h
tape.o: /usr/include/sys/types.h /usr/include/signal.h
tape.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
tape.o: /usr/include/machine/endian.h /usr/include/sys/inode.h
tape.o: /usr/include/sys/fs.h /usr/include/sys/dir.h
tape.o: /usr/include/protocols/dumprestore.h /usr/include/sys/ioctl.h
tape.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tape.o: /usr/include/sys/mtio.h /usr/include/sys/file.h /usr/include/setjmp.h
tape.o: /usr/include/sys/stat.h
utilities.o: utilities.c restore.h /usr/include/stdio.h
utilities.o: /usr/include/sys/param.h /usr/include/sys/types.h
utilities.o: /usr/include/signal.h /usr/include/machine/trap.h
utilities.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
utilities.o: /usr/include/sys/inode.h /usr/include/sys/fs.h
utilities.o: /usr/include/sys/dir.h
dumprmt.o: ../dump/dumprmt.c /usr/include/sys/param.h /usr/include/sys/types.h
dumprmt.o: /usr/include/signal.h /usr/include/machine/trap.h
dumprmt.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
dumprmt.o: /usr/include/sys/mtio.h /usr/include/sys/ioctl.h
dumprmt.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
dumprmt.o: /usr/include/sys/socket.h /usr/include/sys/inode.h
dumprmt.o: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/pwd.h
dumprmt.o: /usr/include/netdb.h /usr/include/protocols/dumprestore.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
