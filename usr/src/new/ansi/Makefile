#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5	(Berkeley)	7/30/87
#
DESTDIR=
CFLAGS=	-O
SRCS=	vmsprep.c ansitape.c
MSRC=	vmsprep.1 ansitape.1
MOBJ=	vmsprep.0 ansitape.0

.SUFFIXES: .1 .0

.1.0:
	/usr/man/manroff $*.1 > $*.0

all: ansitape vmsprep

ansitape:
	${CC} ${CFLAGS} -o $@ ansitape.c

vmsprep:
	${CC} ${CFLAGS} -o $@ vmsprep.c

install: vmsprep ansitape ${MOBJ}
	install -s -o bin -g bin -m 755 vmsprep ${DESTDIR}/usr/new/vmsprep
	install -s -o bin -g bin -m 755 ansitape ${DESTDIR}/usr/new/ansitape
	install -c -o bin -g bin -m 444 vmsprep.0 ${DESTDIR}/usr/new/man/cat1/vmsprep.0
	install -c -o bin -g bin -m 444 ansitape.0 ${DESTDIR}/usr/new/man/cat1/ansitape.0

clean:
	rm -f a.out core errs makefile.bak Makefile.bak ansitape vmsprep *.0

depend:
	mkdep -p ${SRCS}

tags:
	ctags ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

vmsprep: vmsprep.c /usr/include/stdio.h /usr/include/ctype.h
vmsprep: /usr/include/strings.h
ansitape: ansitape.c /usr/include/sys/types.h /usr/include/sys/time.h
ansitape: /usr/include/time.h /usr/include/sys/mtio.h /usr/include/sys/ioctl.h
ansitape: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
ansitape: /usr/include/sys/file.h /usr/include/sys/stat.h /usr/include/a.out.h
ansitape: /usr/include/sys/exec.h /usr/include/stdio.h /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
