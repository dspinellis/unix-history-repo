#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.9	(Berkeley)	6/2/87
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

all: fsck

fsck:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

${LOCOBJS}:
	${CC} ${CFLAGS} -c $*.c

ufs_subr.o:
	${CC} ${CFLAGS} -c /sys/sys/ufs_subr.c

ufs_tables.o:
	${CC} ${CFLAGS} -I/usr/include/sys -c /sys/sys/ufs_tables.c

clean: FRC
	rm -f ${OBJS} fsck core

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 fsck ${DESTDIR}/etc/fsck

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

dir.o: dir.c /usr/include/sys/param.h /usr/include/sys/types.h
dir.o: /usr/include/signal.h /usr/include/machine/trap.h
dir.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
dir.o: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/dir.h
dir.o: fsck.h
inode.o: inode.c /usr/include/pwd.h /usr/include/sys/param.h
inode.o: /usr/include/sys/types.h /usr/include/signal.h
inode.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
inode.o: /usr/include/machine/endian.h /usr/include/sys/inode.h
inode.o: /usr/include/sys/fs.h /usr/include/sys/dir.h fsck.h
main.o: main.c /usr/include/sys/param.h /usr/include/sys/types.h
main.o: /usr/include/signal.h /usr/include/machine/trap.h
main.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
main.o: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/stat.h
main.o: /usr/include/sys/wait.h /usr/include/fstab.h /usr/include/strings.h
main.o: fsck.h
pass1.o: pass1.c /usr/include/sys/param.h /usr/include/sys/types.h
pass1.o: /usr/include/signal.h /usr/include/machine/trap.h
pass1.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
pass1.o: /usr/include/sys/inode.h /usr/include/sys/fs.h fsck.h
pass1b.o: pass1b.c /usr/include/sys/param.h /usr/include/sys/types.h
pass1b.o: /usr/include/signal.h /usr/include/machine/trap.h
pass1b.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
pass1b.o: /usr/include/sys/inode.h /usr/include/sys/fs.h fsck.h
pass2.o: pass2.c /usr/include/sys/param.h /usr/include/sys/types.h
pass2.o: /usr/include/signal.h /usr/include/machine/trap.h
pass2.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
pass2.o: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/dir.h
pass2.o: /usr/include/strings.h fsck.h
pass3.o: pass3.c /usr/include/sys/param.h /usr/include/sys/types.h
pass3.o: /usr/include/signal.h /usr/include/machine/trap.h
pass3.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
pass3.o: /usr/include/sys/inode.h /usr/include/sys/fs.h fsck.h
pass4.o: pass4.c /usr/include/sys/param.h /usr/include/sys/types.h
pass4.o: /usr/include/signal.h /usr/include/machine/trap.h
pass4.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
pass4.o: /usr/include/sys/inode.h /usr/include/sys/fs.h fsck.h
pass5.o: pass5.c /usr/include/sys/param.h /usr/include/sys/types.h
pass5.o: /usr/include/signal.h /usr/include/machine/trap.h
pass5.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
pass5.o: /usr/include/sys/inode.h /usr/include/sys/fs.h fsck.h
setup.o: setup.c /usr/include/sys/param.h /usr/include/sys/types.h
setup.o: /usr/include/signal.h /usr/include/machine/trap.h
setup.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
setup.o: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/stat.h
setup.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
setup.o: /usr/include/sys/ttydev.h /usr/include/sys/disklabel.h
setup.o: /usr/include/sys/file.h /usr/include/machine/endian.h
setup.o: /usr/include/ctype.h fsck.h
utilities.o: utilities.c /usr/include/stdio.h /usr/include/ctype.h
utilities.o: /usr/include/sys/param.h /usr/include/sys/types.h
utilities.o: /usr/include/signal.h /usr/include/machine/trap.h
utilities.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
utilities.o: /usr/include/sys/inode.h /usr/include/sys/fs.h
utilities.o: /usr/include/sys/dir.h fsck.h
ufs_subr.o: /sys/sys/ufs_subr.c /usr/include/sys/param.h
ufs_subr.o: /usr/include/sys/types.h /usr/include/signal.h
ufs_subr.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
ufs_subr.o: /usr/include/machine/endian.h /usr/include/sys/systm.h
ufs_subr.o: /usr/include/sys/mount.h /usr/include/sys/fs.h
ufs_subr.o: /usr/include/sys/buf.h /usr/include/sys/inode.h
ufs_subr.o: /usr/include/sys/dir.h /usr/include/sys/user.h
ufs_subr.o: /usr/include/machine/pcb.h /usr/include/sys/dmap.h
ufs_subr.o: /usr/include/sys/time.h /usr/include/time.h
ufs_subr.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
ufs_subr.o: /usr/include/sys/uio.h /usr/include/errno.h
ufs_subr.o: /usr/include/sys/quota.h
ufs_tables.o: /sys/sys/ufs_tables.c /usr/include/sys/param.h
ufs_tables.o: /usr/include/sys/types.h /usr/include/signal.h
ufs_tables.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
ufs_tables.o: /usr/include/machine/endian.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
