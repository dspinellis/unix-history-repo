#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.7 (Berkeley) %G%
#
DESTDIR=
CFLAGS=	-O
LFLAGS=	

# The program itself
#
PROG=	fsck

# Sources
#
SRCS=	dir.c inode.c main.c pass1.c pass1b.c pass2.c \
	pass3.c pass4.c pass5.c setup.c utilities.c \
	/sys/sys/ufs_subr.c /sys/sys/ufs_tables.c

# Objects
#
OBJS=	${LOCOBJS} ${SYSOBJS}
LOCOBJS=dir.o inode.o main.o pass1.o pass1b.o pass2.o \
	pass3.o pass4.o pass5.o setup.o utilities.o
SYSOBJS=ufs_subr.o ufs_tables.o

# Header files
#
HDRS=	fsck.h

${PROG}: ${OBJS} ${HDRS}
	cc ${LFLAGS} -o ${PROG} ${OBJS}

${LOCOBJS}:
	cc ${CFLAGS} -c $*.c

ufs_subr.o:
	cc ${CFLAGS} -c /sys/sys/ufs_subr.c

ufs_tables.o:
	cc ${CFLAGS} -I/usr/include/sys -c /sys/sys/ufs_tables.c

install: ${PROG}
	install ${PROG} ${DESTDIR}/etc/${PROG}

clean:
	rm -f a.out core ${OBJS} ${PROG}

lint:
	lint ${INCPATH} ${SRCS}

depend:
	cc -M ${INCPATH} ${SRCS} | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it


dir.o: dir.c /usr/include/sys/param.h /usr/include/machine/machparam.h
dir.o: /usr/include/sys/signal.h /usr/include/sys/types.h
dir.o: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/dir.h
dir.o: ./fsck.h
inode.o: inode.c /usr/include/sys/param.h /usr/include/machine/machparam.h
inode.o: /usr/include/sys/signal.h /usr/include/sys/types.h
inode.o: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/dir.h
inode.o: ./fsck.h
main.o: main.c /usr/include/sys/param.h /usr/include/machine/machparam.h
main.o: /usr/include/sys/signal.h /usr/include/sys/types.h
main.o: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/stat.h
main.o: /usr/include/sys/wait.h /usr/include/fstab.h /usr/include/strings.h
main.o: ./fsck.h
pass1.o: pass1.c /usr/include/sys/param.h /usr/include/machine/machparam.h
pass1.o: /usr/include/sys/signal.h /usr/include/sys/types.h
pass1.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
pass1b.o: pass1b.c /usr/include/sys/param.h /usr/include/machine/machparam.h
pass1b.o: /usr/include/sys/signal.h /usr/include/sys/types.h
pass1b.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
pass2.o: pass2.c /usr/include/sys/param.h /usr/include/machine/machparam.h
pass2.o: /usr/include/sys/signal.h /usr/include/sys/types.h
pass2.o: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/dir.h
pass2.o: /usr/include/strings.h ./fsck.h
pass3.o: pass3.c /usr/include/sys/param.h /usr/include/machine/machparam.h
pass3.o: /usr/include/sys/signal.h /usr/include/sys/types.h
pass3.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
pass4.o: pass4.c /usr/include/sys/param.h /usr/include/machine/machparam.h
pass4.o: /usr/include/sys/signal.h /usr/include/sys/types.h
pass4.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
pass5.o: pass5.c /usr/include/sys/param.h /usr/include/machine/machparam.h
pass5.o: /usr/include/sys/signal.h /usr/include/sys/types.h
pass5.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
setup.o: setup.c /usr/include/sys/param.h /usr/include/machine/machparam.h
setup.o: /usr/include/sys/signal.h /usr/include/sys/types.h
setup.o: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/stat.h
setup.o: ./fsck.h
utilities.o: utilities.c /usr/include/stdio.h /usr/include/ctype.h
utilities.o: /usr/include/sys/param.h /usr/include/machine/machparam.h
utilities.o: /usr/include/sys/signal.h /usr/include/sys/types.h
utilities.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
ufs_subr.o: /sys/sys/ufs_subr.c /usr/include/sys/param.h
ufs_subr.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
ufs_subr.o: /usr/include/sys/types.h /usr/include/sys/systm.h
ufs_subr.o: /usr/include/sys/mount.h /usr/include/sys/fs.h
ufs_subr.o: /usr/include/sys/conf.h /usr/include/sys/buf.h
ufs_subr.o: /usr/include/sys/inode.h /usr/include/sys/dir.h
ufs_subr.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
ufs_subr.o: /usr/include/sys/dmap.h /usr/include/sys/time.h
ufs_subr.o: /usr/include/sys/resource.h /usr/include/sys/namei.h
ufs_subr.o: /usr/include/sys/uio.h /usr/include/sys/errno.h
ufs_subr.o: /usr/include/sys/quota.h
ufs_tables.o: /sys/sys/ufs_tables.c /usr/include/sys/param.h
ufs_tables.o: /usr/include/machine/machparam.h /usr/include/sys/signal.h
ufs_tables.o: /usr/include/sys/types.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
