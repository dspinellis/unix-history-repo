#
# Copyright (c) 1987 The Regents of the University of California.
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
#	@(#)Makefile	5.10 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	dir.c inode.c main.c pass1.c pass1b.c pass2.c pass3.c \
	pass4.c pass5.c setup.c utilities.c /sys/sys/ufs_subr.c \
	/sys/sys/ufs_tables.c
LOCOBJS=dir.o inode.o main.o pass1.o pass1b.o pass2.o \
	pass3.o pass4.o pass5.o setup.o utilities.o
SYSOBJS=ufs_subr.o ufs_tables.o
OBJS=	${LOCOBJS} ${SYSOBJS}
MAN=	fsck.0

all: fsck

fsck: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

${LOCOBJS}:
	${CC} ${CFLAGS} -c $*.c

ufs_subr.o:
	${CC} ${CFLAGS} -c /sys/sys/ufs_subr.c

ufs_tables.o:
	${CC} ${CFLAGS} -I/usr/include/sys -c /sys/sys/ufs_tables.c

clean:
	rm -f ${OBJS} fsck core

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 fsck ${DESTDIR}/etc/fsck
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat8

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
