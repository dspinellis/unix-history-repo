#	@(#)Makefile	4.3	(Berkeley)	83/01/09
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

install: fsck
	install fsck ${DESTDIR}/etc/fsck

clean:
	rm -f fsck fscklc.o fsck.o ufs_subr.o ufs_tables.o
	rm -f errs a.out core

depend:
	for i in fsck; do \
		(echo $$i: $$i.c >>makedep; \
		/bin/grep '^#[ 	]*include' $$i.c | sed \
			-e '/\.\.\/h/d' \
			-e 's,<\(.*\)>,"/usr/include/\1",' \
			-e 's/:[^"]*"\([^"]*\)".*/: \1/' \
			-e 's/\.c//' >>makedep); done
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

fsck: fsck.c
#include "/usr/include/stdio.h"
#include "/usr/include/ctype.h"
#include "/usr/include/sys/param.h"
#include "/usr/include/sys/fs.h"
#include "/usr/include/sys/inode.h"
#include "/usr/include/dir.h"
#include "/usr/include/sys/stat.h"
#include "/usr/include/fstab.h"
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
