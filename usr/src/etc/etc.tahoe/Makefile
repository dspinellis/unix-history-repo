#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	1.2	(Berkeley)	12/10/87
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	enpload.c
ALL=	enpload
SUBDIR=	dlmpcc

all:	${ALL} ${SUBDIR}

${ALL}: ${LIBC}
	${CC} ${CFLAGS} -o $@ $@.c

${SUBDIR}: FRC
	cd $@; make MFLAGS=${MFLAGS}

clean: FRC
	rm -f ${ALL} core
	for i in ${SUBDIR} ; do \
		(cd $$i; make ${MFLAGS} clean); \
	done

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}
	for i in ${SUBDIR} ; do \
		(cd $$i; make ${MFLAGS} depend); \
	done

install: FRC
	install -s -o bin -g bin -m 755 enpload ${DESTDIR}/etc/enpload
	install -c -o bin -g bin -m 444 enpcode ${DESTDIR}/etc/enpcode
	for i in ${SUBDIR} ; do \
		(cd $$i; make ${MFLAGS} install); \
	done

tags:
	for i in ${SUBDIR} ; do \
		(cd $$i; make ${MFLAGS} tags); \
	done

lint:
FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

enpload: enpload.c /usr/include/stdio.h /usr/include/a.out.h
enpload: /usr/include/sys/exec.h /usr/include/sys/types.h
enpload: /usr/include/sys/file.h /usr/include/sys/ioctl.h
enpload: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
enpload: /usr/include/tahoeif/if_enpreg.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
