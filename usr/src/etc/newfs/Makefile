#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.3	(Berkeley)	6/2/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	newfs.c mkfs.c
OBJS=	newfs.o mkfs.o

all: newfs

newfs: ${OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${OBJS}

clean: FRC
	rm -f ${OBJS} core newfs

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 newfs ${DESTDIR}/etc/newfs

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

newfs.o: newfs.c /usr/include/sys/param.h /usr/include/sys/types.h
newfs.o: /usr/include/signal.h /usr/include/machine/trap.h
newfs.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
newfs.o: /usr/include/sys/stat.h /usr/include/sys/fs.h /usr/include/sys/dir.h
newfs.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
newfs.o: /usr/include/sys/ttydev.h /usr/include/sys/disklabel.h
newfs.o: /usr/include/sys/file.h /usr/include/stdio.h /usr/include/ctype.h
mkfs.o: mkfs.c /usr/include/stdio.h /usr/include/a.out.h
mkfs.o: /usr/include/sys/exec.h /usr/include/sys/param.h
mkfs.o: /usr/include/sys/types.h /usr/include/signal.h
mkfs.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
mkfs.o: /usr/include/machine/endian.h /usr/include/sys/inode.h
mkfs.o: /usr/include/sys/fs.h /usr/include/sys/dir.h
mkfs.o: /usr/include/sys/disklabel.h /usr/include/machine/endian.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
