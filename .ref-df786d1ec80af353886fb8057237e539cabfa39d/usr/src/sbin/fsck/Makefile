#	@(#)Makefile	4.4	(Berkeley)	83/09/10
#
COPT=
CFLAGS=-O

fsck: fsck.o ufs_subr.o ufs_tables.o
	cc ${COPT} -o fsck fsck.o ufs_subr.o ufs_tables.o

fscklc.o: fsck.c
	cc ${COPT} ${CFLAGS} -c fsck.c
	ld -r -o fscklc.o fsck.o -lc

ufs_subr.o: /sys/sys/ufs_subr.c
	cc ${COPT} -c /sys/sys/ufs_subr.c

ufs_tables.o: /sys/sys/ufs_tables.c
	cc ${COPT} -c /sys/sys/ufs_tables.c

lint:
	lint fsck.c

install: fsck
	install fsck ${DESTDIR}/etc/fsck

clean:
	rm -f fsck fscklc.o fsck.o ufs_subr.o ufs_tables.o
	rm -f errs a.out core

depend:
	echo >x.c
	for i in fsck; do \
		(echo $$i: $$i.c >>makedep; \
		/bin/grep '^#[ 	]*include' x.c $$i.c | sed \
			-e '/\.\.\/h/d' \
			-e 's,<\(.*\)>,"/usr/include/\1",' \
			-e 's/:[^"]*"\([^"]*\)".*/: \1/' \
			-e 's/\.c//' >>makedep); done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep x.c
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

fsck: fsck.c
fsck: /usr/include/stdio.h
fsck: /usr/include/ctype.h
fsck: /usr/include/sys/param.h
fsck: /usr/include/sys/fs.h
fsck: /usr/include/sys/inode.h
fsck: /usr/include/sys/dir.h
fsck: /usr/include/sys/stat.h
fsck: /usr/include/sys/wait.h
fsck: /usr/include/fstab.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
