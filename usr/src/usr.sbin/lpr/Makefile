#
# Copyright (c) 1987 Regents of the University of California.
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
#	@(#)Makefile	5.11 (Berkeley) %G%
#
# Ownerships {see note at the top of lpd.c}
# ROOT		a user that can access any and all files
# DAEMON	someone special
# SPGRP		the group id of the spooling programs
#
CFLAGS=	-O
SPLDIR=	/var/spool/lpd
DAEMON=	daemon
SPGRP=	daemon
LIBC=	/lib/libc.a
L1SRCS=	lpd.c printjob.c recvjob.c displayq.c rmjob.c startdaemon.c \
	lpdchar.c common.c printcap.c
L1OBJS=	lpd.o printjob.o recvjob.o displayq.o rmjob.o startdaemon.o \
	lpdchar.o common.o printcap.o
L2SRCS=	lpr.c startdaemon.c printcap.c
L2OBJS=	lpr.o startdaemon.o printcap.o
L3SRCS=	lpq.c displayq.c common.c printcap.c
L3OBJS=	lpq.o displayq.o common.o printcap.o
L4SRCS=	lprm.c rmjob.c startdaemon.c common.c printcap.c
L4OBJS=	lprm.o rmjob.o startdaemon.o common.o printcap.o
L5SRCS=	lpc.c cmds.c cmdtab.c startdaemon.c common.c printcap.c
L5OBJS=	lpc.o cmds.o cmdtab.o startdaemon.o common.o printcap.o
L6SRCS=	lptest.c
L6OBJS=	lptest.o
L7SRCS=	pac.c printcap.c
L7OBJS=	pac.o printcap.o
SRCS=	lpd.c lpr.c lpq.c lprm.c pac.c lpd.c cmds.c cmdtab.c printjob.c \
	recvjob.c displayq.c rmjob.c startdaemon.c common.c printcap.c \
	lpdchar.c
OBJS=	${L1OBJS} ${L2OBJS} ${L3OBJS} ${L4OBJS} ${L5OBJS} ${L6OBJS} ${L7OBJS}
MAN1=	lpq.0 lpr.0 lprm.0 lptest.0
MAN8=	lpc.0 lpd.0 pac.0
MAN=	${MAN1} ${MAN8}
ALL=	lpd lpr lpq lprm lpc lptest pac

all: ${ALL} FILTERS VFILTERS

lpd: ${L1OBJS} ${LIBC}
	${CC} -o $@ ${L1OBJS}

lpr: ${L2OBJS} ${LIBC}
	${CC} -o $@ ${L2OBJS}

lpq: ${L3OBJS} ${LIBC}
	${CC} -o $@ ${L3OBJS}

lprm: ${L4OBJS} ${LIBC}
	${CC} -o $@ ${L4OBJS}

lpc: ${L5OBJS} ${LIBC}
	${CC} -o $@ ${L5OBJS}

lptest: ${L6OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${L6OBJS}

pac: ${L7OBJS} ${LIBC}
	${CC} -o $@ ${L7OBJS}

FILTERS:
	cd filters; make ${MFLAGS}

VFILTERS:
	cd vfilters; make ${MFLAGS}

clean:
	rm -f ${OBJS} core ${ALL}
	cd filters; make ${MFLAGS} clean
	cd vfilters; make ${MFLAGS} clean

cleandir: clean
	rm -f ${MAN} tags .depend
	cd filters; make ${MFLAGS} clean
	cd vfilters; make ${MFLAGS} clean

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}
	cd filters; make ${MFLAGS} depend
	cd vfilters; make ${MFLAGS} depend

install: ${MAN}
	install -s -o root -g ${SPGRP} -m 6711 lpd ${DESTDIR}/usr/sbin
	install -s -o root -g ${SPGRP} -m 6711 lpr lpq lprm \
	  ${DESTDIR}/usr/bin
	install -s -o bin -g ${SPGRP} -m 2711 lpc ${DESTDIR}/usr/sbin
	install -s -o bin -g bin -m 755 lptest pac ${DESTDIR}/usr/sbin
	chown ${DAEMON} ${DESTDIR}/${SPLDIR}
	chgrp ${SPGRP} ${DESTDIR}/${SPLDIR}
	chmod 775 ${DESTDIR}/${SPLDIR}
	install -c -o bin -g bin -m 444 ${MAN1} ${DESTDIR}/usr/man/cat1
	install -c -o bin -g bin -m 444 ${MAN8} ${DESTDIR}/usr/man/cat8
	cd filters; make ${MFLAGS} DESTDIR=${DESTDIR} install
	cd vfilters; make ${MFLAGS} DESTDIR=${DESTDIR} install

lint: ${SRCS}
	lint ${CFLAGS} ${L1SRCS}
	lint ${CFLAGS} ${L2SRCS}
	lint ${CFLAGS} ${L3SRCS}
	lint ${CFLAGS} ${L4SRCS}
	lint ${CFLAGS} ${L5SRCS}
	lint ${CFLAGS} ${L6SRCS}
	lint ${CFLAGS} ${L7SRCS}

tags: ${SRCS}
	ctags ${L1SRCS}
	ctags -a ${L2SRCS}
	ctags -a ${L3SRCS}
	ctags -a ${L4SRCS}
	ctags -a ${L5SRCS}
	ctags -a ${L6SRCS}
	ctags -a ${L7SRCS}
	sort -o tags tags
