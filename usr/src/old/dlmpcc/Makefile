#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a

all:	dlmpcc

dlmpcc: dlmpcc.c ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

clean:
	rm -f dlmpcc core a.out errs

depend:
	mkdep -p ${CFLAGS} dlmpcc.c

install: dlmpcc
	install -s -o bin -g bin -m 755 dlmpcc ${DESTDIR}/etc/dlmpcc

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

dlmpcc: dlmpcc.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/fcntl.h
dlmpcc: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
dlmpcc: /usr/include/sys/ttydev.h /usr/include/errno.h /usr/include/sys/types.h
dlmpcc: /usr/include/tahoevba/mpreg.h /usr/include/stdio.h scnhdr.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
