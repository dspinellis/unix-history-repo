#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.6	(Berkeley)	6/3/87
#
#	dump.h			header file
#	dumpitime.c		reads /etc/dumpdates
#	dumpmain.c		driver
#	dumpoptr.c		operator interface
#	dumptape.c		handles the mag tape and opening/closing
#	dumptraverse.c		traverses the file system
#	unctime.c		undo ctime
#
#	DEBUG			use local directory to find ddate and dumpdates
#	TDEBUG			trace out the process forking

DFLAGS=
CFLAGS=	-O ${DFLAGS}
LIBC=	/lib/libc.a
PSRCS=	/usr/include/protocols/dumprestore.h dump.h dumpmain.c \
	dumptraverse.c dumptape.c dumpoptr.c dumpitime.c unctime.c
SRCS=	dump.h dumpitime.c dumpmain.c dumpoptr.c dumprmt.c dumptape.c \
	dumptraverse.c unctime.c
OBJS=	dumpitime.o dumpmain.o dumpoptr.o dumptape.o dumptraverse.o \
	unctime.o
ROBJS=	dumpitime.o dumprmain.o dumpoptr.o dumprmt.o dumprtape.o \
	dumptraverse.o unctime.o

all: dump rdump

dump:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

rdump:	${ROBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${ROBJS}

dumprmain.o: dumpmain.c
	${CC} -c -DRDUMP ${CFLAGS} dumpmain.c -o dumprmain.o

dumprtape.o: dumptape.c
	${CC} -c -DRDUMP ${CFLAGS} dumptape.c -o dumprtape.o

clean: FRC
	rm -f ${ROBJS} ${OBJS} rdump dump core

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o root -g tty -m 6755 rdump ${DESTDIR}/etc/rdump
	install -s -o bin -g tty -m 2755 dump ${DESTDIR}/etc/dump

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

dump.o: dump.h /usr/include/stdio.h /usr/include/ctype.h
dump.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
dump.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
dump.o: /usr/include/machine/endian.h /usr/include/sys/stat.h
dump.o: /usr/include/sys/fs.h /usr/include/sys/inode.h
dump.o: /usr/include/protocols/dumprestore.h /usr/include/sys/dir.h
dump.o: /usr/include/utmp.h /usr/include/sys/time.h /usr/include/time.h
dump.o: /usr/include/signal.h /usr/include/fstab.h
dumpitime.o: dumpitime.c dump.h /usr/include/stdio.h /usr/include/ctype.h
dumpitime.o: /usr/include/sys/param.h /usr/include/sys/types.h
dumpitime.o: /usr/include/signal.h /usr/include/machine/trap.h
dumpitime.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
dumpitime.o: /usr/include/sys/stat.h /usr/include/sys/fs.h
dumpitime.o: /usr/include/sys/inode.h /usr/include/protocols/dumprestore.h
dumpitime.o: /usr/include/sys/dir.h /usr/include/utmp.h /usr/include/sys/time.h
dumpitime.o: /usr/include/time.h /usr/include/signal.h /usr/include/fstab.h
dumpitime.o: /usr/include/sys/file.h
dumpmain.o: dumpmain.c dump.h /usr/include/stdio.h /usr/include/ctype.h
dumpmain.o: /usr/include/sys/param.h /usr/include/sys/types.h
dumpmain.o: /usr/include/signal.h /usr/include/machine/trap.h
dumpmain.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
dumpmain.o: /usr/include/sys/stat.h /usr/include/sys/fs.h
dumpmain.o: /usr/include/sys/inode.h /usr/include/protocols/dumprestore.h
dumpmain.o: /usr/include/sys/dir.h /usr/include/utmp.h /usr/include/sys/time.h
dumpmain.o: /usr/include/time.h /usr/include/signal.h /usr/include/fstab.h
dumpoptr.o: dumpoptr.c dump.h /usr/include/stdio.h /usr/include/ctype.h
dumpoptr.o: /usr/include/sys/param.h /usr/include/sys/types.h
dumpoptr.o: /usr/include/signal.h /usr/include/machine/trap.h
dumpoptr.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
dumpoptr.o: /usr/include/sys/stat.h /usr/include/sys/fs.h
dumpoptr.o: /usr/include/sys/inode.h /usr/include/protocols/dumprestore.h
dumpoptr.o: /usr/include/sys/dir.h /usr/include/utmp.h /usr/include/sys/time.h
dumpoptr.o: /usr/include/time.h /usr/include/signal.h /usr/include/fstab.h
dumprmt.o: dumprmt.c /usr/include/sys/param.h /usr/include/sys/types.h
dumprmt.o: /usr/include/signal.h /usr/include/machine/trap.h
dumprmt.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
dumprmt.o: /usr/include/sys/mtio.h /usr/include/sys/ioctl.h
dumprmt.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
dumprmt.o: /usr/include/sys/socket.h /usr/include/sys/inode.h
dumprmt.o: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/pwd.h
dumprmt.o: /usr/include/netdb.h /usr/include/protocols/dumprestore.h
dumptape.o: dumptape.c /usr/include/sys/file.h dump.h /usr/include/stdio.h
dumptape.o: /usr/include/ctype.h /usr/include/sys/param.h
dumptape.o: /usr/include/sys/types.h /usr/include/signal.h
dumptape.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
dumptape.o: /usr/include/machine/endian.h /usr/include/sys/stat.h
dumptape.o: /usr/include/sys/fs.h /usr/include/sys/inode.h
dumptape.o: /usr/include/protocols/dumprestore.h /usr/include/sys/dir.h
dumptape.o: /usr/include/utmp.h /usr/include/sys/time.h /usr/include/time.h
dumptape.o: /usr/include/signal.h /usr/include/fstab.h
dumptraverse.o: dumptraverse.c dump.h /usr/include/stdio.h /usr/include/ctype.h
dumptraverse.o: /usr/include/sys/param.h /usr/include/sys/types.h
dumptraverse.o: /usr/include/signal.h /usr/include/machine/trap.h
dumptraverse.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
dumptraverse.o: /usr/include/sys/stat.h /usr/include/sys/fs.h
dumptraverse.o: /usr/include/sys/inode.h /usr/include/protocols/dumprestore.h
dumptraverse.o: /usr/include/sys/dir.h /usr/include/utmp.h
dumptraverse.o: /usr/include/sys/time.h /usr/include/time.h
dumptraverse.o: /usr/include/signal.h /usr/include/fstab.h
unctime.o: unctime.c /usr/include/sys/types.h /usr/include/sys/time.h
unctime.o: /usr/include/time.h /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
