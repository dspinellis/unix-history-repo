#	@(#)Makefile	5.2	(Berkeley)	84/08/06
#
DESTDIR=
CFLAGS=	-O
INCPATH=
LFLAGS=	

# The program itself
#
PROG=	fsck

# Sources
#
SRCS=	dir.c fsck.c inode.c main.c pass1.c pass1b.c pass2.c \
	pass3.c pass4.c pass5.c pass6.c setup.c utilities.c \
	/sys/sys/ufs_subr.c /sys/sys/ufs_tables.c

# Objects
#
OBJS=	${LOCOBJS} ${SYSOBJS}
LOCOBJS=dir.o fsck.o inode.o main.o pass1.o pass1b.o pass2.o \
	pass3.o pass4.o pass5.o pass6.o setup.o utilities.o
SYSOBJS=ufs_subr.o ufs_tables.o

# Header files
#
HDRS=	fsck.h

${PROG}: ${OBJS} ${HDRS}
	cc ${LFLAGS} -o ${PROG} ${OBJS}

${LOCOBJS}:
	cc ${CFLAGS} ${INCPATH} -c $*.c

ufs_subr.o:
	cc ${CFLAGS} ${INCPATH} -c /sys/sys/ufs_subr.c

ufs_tables.o:
	cc ${CFLAGS} ${INCPATH} -c /sys/sys/ufs_tables.c

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


dir.o: dir.c /usr/include/sys/param.h /usr/include/machine/param.h
dir.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/inode.h
dir.o: /usr/include/sys/fs.h /usr/include/sys/dir.h ./fsck.h
fsck.o: fsck.c /usr/include/sys/param.h /usr/include/machine/param.h
fsck.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/inode.h
fsck.o: /usr/include/sys/fs.h ./fsck.h
inode.o: inode.c /usr/include/sys/param.h /usr/include/machine/param.h
inode.o: /usr/include/signal.h /usr/include/sys/types.h
inode.o: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/dir.h
inode.o: ./fsck.h
main.o: main.c /usr/include/sys/param.h /usr/include/machine/param.h
main.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/inode.h
main.o: /usr/include/sys/fs.h /usr/include/sys/stat.h /usr/include/sys/wait.h
main.o: /usr/include/fstab.h /usr/include/strings.h ./fsck.h
pass1.o: pass1.c /usr/include/sys/param.h /usr/include/machine/param.h
pass1.o: /usr/include/signal.h /usr/include/sys/types.h
pass1.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
pass1b.o: pass1b.c /usr/include/sys/param.h /usr/include/machine/param.h
pass1b.o: /usr/include/signal.h /usr/include/sys/types.h
pass1b.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
pass2.o: pass2.c /usr/include/sys/param.h /usr/include/machine/param.h
pass2.o: /usr/include/signal.h /usr/include/sys/types.h
pass2.o: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/dir.h
pass2.o: /usr/include/strings.h ./fsck.h
pass3.o: pass3.c /usr/include/sys/param.h /usr/include/machine/param.h
pass3.o: /usr/include/signal.h /usr/include/sys/types.h
pass3.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
pass4.o: pass4.c /usr/include/sys/param.h /usr/include/machine/param.h
pass4.o: /usr/include/signal.h /usr/include/sys/types.h
pass4.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
pass5.o: pass5.c /usr/include/sys/param.h /usr/include/machine/param.h
pass5.o: /usr/include/signal.h /usr/include/sys/types.h
pass5.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
pass6.o: pass6.c /usr/include/sys/param.h /usr/include/machine/param.h
pass6.o: /usr/include/signal.h /usr/include/sys/types.h
pass6.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
setup.o: setup.c /usr/include/sys/param.h /usr/include/machine/param.h
setup.o: /usr/include/signal.h /usr/include/sys/types.h
setup.o: /usr/include/sys/inode.h /usr/include/sys/fs.h /usr/include/sys/stat.h
setup.o: ./fsck.h
utilities.o: utilities.c /usr/include/stdio.h /usr/include/ctype.h
utilities.o: /usr/include/sys/param.h /usr/include/machine/param.h
utilities.o: /usr/include/signal.h /usr/include/sys/types.h
utilities.o: /usr/include/sys/inode.h /usr/include/sys/fs.h ./fsck.h
ufs_subr.o: /sys/sys/ufs_subr.c /usr/include/sys/param.h
ufs_subr.o: /usr/include/machine/param.h /usr/include/signal.h
ufs_subr.o: /usr/include/sys/types.h /usr/include/sys/systm.h
ufs_subr.o: /usr/include/sys/mount.h /usr/include/sys/fs.h
ufs_subr.o: /usr/include/sys/conf.h /usr/include/sys/buf.h
ufs_subr.o: /usr/include/sys/inode.h /usr/include/sys/dir.h
ufs_subr.o: /usr/include/sys/user.h /usr/include/machine/pcb.h
ufs_subr.o: /usr/include/sys/dmap.h /usr/include/sys/time.h
ufs_subr.o: /usr/include/sys/resource.h /usr/include/errno.h
ufs_subr.o: /usr/include/sys/quota.h
ufs_tables.o: /sys/sys/ufs_tables.c /sys/sys/../h/param.h
ufs_tables.o: /usr/include/machine/param.h /usr/include/signal.h
ufs_tables.o: /usr/include/sys/types.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
