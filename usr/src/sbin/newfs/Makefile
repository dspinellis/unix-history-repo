#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.1 (Berkeley) %G%
#
DESTDIR=
CFLAGS=	-O
LDFLAGS=	

SRCS=	newfs.c mkfs.c
OBJS=	newfs.o mkfs.o

all: newfs

newfs: ${OBJS}}
	cc ${LDFLAGS} ${CFLAGS} -o newfs ${OBJS}}

install: all
	install -s newfs ${DESTDIR}/etc/newfs

clean:
	rm -f newfs *.o errs a.out core

lint:
	lint ${SRCS}

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


newfs.o: newfs.c /usr/include/sys/param.h /usr/include/machine/machparam.h
newfs.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
newfs.o: /usr/include/sys/fs.h /usr/include/sys/dir.h /usr/include/sys/ioctl.h
newfs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
newfs.o: /usr/include/sys/disklabel.h /usr/include/sys/file.h
newfs.o: /usr/include/stdio.h /usr/include/ctype.h
mkfs.o: mkfs.c /usr/include/stdio.h /usr/include/a.out.h
mkfs.o: /usr/include/sys/exec.h /usr/include/sys/param.h
mkfs.o: /usr/include/machine/machparam.h /usr/include/signal.h
mkfs.o: /usr/include/sys/types.h /usr/include/sys/inode.h /usr/include/sys/fs.h
mkfs.o: /usr/include/sys/dir.h /usr/include/sys/disklabel.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
