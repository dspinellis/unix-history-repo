#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.6	(Berkeley)	%G%
#
# makefile for line printer spooling system
#
# Ownerships {see note at the top of lpd.c}
# ROOT		a user that can access any and all files
# DAEMON	someone special
# SPGRP		the group id of the spooling programs
#
CFLAGS=	-O
LIBDIR=	/usr/lib
BINDIR=	/usr/ucb
SPLDIR=	/usr/spool/lpd
ROOT=	root
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
ALL=	lpd lpr lpq lprm lpc lptest pac

all:	${ALL} FILTERS

lpd:	${L1OBJS} ${LIBC}
	${CC} -o $@ ${L1OBJS}

lpr:	${L2OBJS} ${LIBC}
	${CC} -o $@ ${L2OBJS}

lpq:	${L3OBJS} ${LIBC}
	${CC} -o $@ ${L3OBJS}

lprm:	${L4OBJS} ${LIBC}
	${CC} -o $@ ${L4OBJS}

lpc:	${L5OBJS} ${LIBC}
	${CC} -o $@ ${L5OBJS}

lptest: ${L6OBJS} ${LIBC}
	${CC} ${CFLAGS} -o $@ ${L6OBJS}

pac: ${L7OBJS} ${LIBC}
	${CC} -o $@ ${L7OBJS}

FILTERS:
	cd filters; make ${MFLAGS}

clean: FRC
	rm -f ${OBJS} core ${ALL}
	cd filters; make ${MFLAGS} clean

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o ${ROOT} -g ${SPGRP} -m 6711 lpd ${DESTDIR}/${LIBDIR}/lpd
	install -s -o ${ROOT} -g ${SPGRP} -m 6711 lpr ${DESTDIR}/${BINDIR}/lpr
	install -s -o ${ROOT} -g ${SPGRP} -m 6711 lpq ${DESTDIR}/${BINDIR}/lpq
	install -s -o ${ROOT} -g ${SPGRP} -m 6711 lprm ${DESTDIR}/${BINDIR}/lprm
	install -s -o bin -g ${SPGRP} -m 2711 lpc ${DESTDIR}/etc/lpc
	install -s -o bin -g bin -m 755 lptest ${DESTDIR}/${BINDIR}/lptest
	install -s -o bin -g bin -m 755 pac ${DESTDIR}/etc/pac
	/etc/chown ${DAEMON} ${DESTDIR}/${SPLDIR}
	chgrp ${SPGRP} ${DESTDIR}/${SPLDIR}
	chmod 775 ${DESTDIR}/${SPLDIR}
	cd filters; make ${MFLAGS} DESTDIR=${DESTDIR} install

lint: FRC
	lint ${CFLAGS} ${L1SRCS}
	lint ${CFLAGS} ${L2SRCS}
	lint ${CFLAGS} ${L3SRCS}
	lint ${CFLAGS} ${L4SRCS}
	lint ${CFLAGS} ${L5SRCS}
	lint ${CFLAGS} ${L6SRCS}
	lint ${CFLAGS} ${L7SRCS}

tags: FRC
	ctags ${L1SRCS}
	ctags -a ${L2SRCS}
	ctags -a ${L3SRCS}
	ctags -a ${L4SRCS}
	ctags -a ${L5SRCS}
	ctags -a ${L6SRCS}
	ctags -a ${L7SRCS}
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

lpd.o: lpd.c lp.h /usr/include/stdio.h /usr/include/sys/param.h
lpd.o: /usr/include/sys/types.h /usr/include/signal.h
lpd.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
lpd.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/sys/socket.h
lpd.o: /usr/include/sys/un.h /usr/include/netinet/in.h /usr/include/netdb.h
lpd.o: /usr/include/pwd.h /usr/include/syslog.h /usr/include/signal.h
lpd.o: /usr/include/sys/wait.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
lpd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
lpd.o: /usr/include/ctype.h /usr/include/errno.h lp.local.h
lpd.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ar.h
lpr.o: lpr.c /usr/include/stdio.h /usr/include/sys/types.h
lpr.o: /usr/include/sys/file.h /usr/include/sys/stat.h /usr/include/pwd.h
lpr.o: /usr/include/grp.h /usr/include/signal.h /usr/include/ctype.h
lpr.o: /usr/include/syslog.h lp.local.h /usr/include/a.out.h
lpr.o: /usr/include/sys/exec.h /usr/include/ar.h
lpq.o: lpq.c lp.h /usr/include/stdio.h /usr/include/sys/param.h
lpq.o: /usr/include/sys/types.h /usr/include/signal.h
lpq.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
lpq.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/sys/socket.h
lpq.o: /usr/include/sys/un.h /usr/include/netinet/in.h /usr/include/netdb.h
lpq.o: /usr/include/pwd.h /usr/include/syslog.h /usr/include/signal.h
lpq.o: /usr/include/sys/wait.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
lpq.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
lpq.o: /usr/include/ctype.h /usr/include/errno.h lp.local.h
lpq.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ar.h
lprm.o: lprm.c lp.h /usr/include/stdio.h /usr/include/sys/param.h
lprm.o: /usr/include/sys/types.h /usr/include/signal.h
lprm.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
lprm.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
lprm.o: /usr/include/sys/socket.h /usr/include/sys/un.h
lprm.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/pwd.h
lprm.o: /usr/include/syslog.h /usr/include/signal.h /usr/include/sys/wait.h
lprm.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
lprm.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
lprm.o: /usr/include/ctype.h /usr/include/errno.h lp.local.h
lprm.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ar.h
pac.o: pac.c /usr/include/stdio.h lp.local.h /usr/include/a.out.h
pac.o: /usr/include/sys/exec.h /usr/include/ar.h
lpd.o: lpd.c lp.h /usr/include/stdio.h /usr/include/sys/param.h
lpd.o: /usr/include/sys/types.h /usr/include/signal.h
lpd.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
lpd.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/sys/socket.h
lpd.o: /usr/include/sys/un.h /usr/include/netinet/in.h /usr/include/netdb.h
lpd.o: /usr/include/pwd.h /usr/include/syslog.h /usr/include/signal.h
lpd.o: /usr/include/sys/wait.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
lpd.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
lpd.o: /usr/include/ctype.h /usr/include/errno.h lp.local.h
lpd.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ar.h
cmds.o: cmds.c lp.h /usr/include/stdio.h /usr/include/sys/param.h
cmds.o: /usr/include/sys/types.h /usr/include/signal.h
cmds.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
cmds.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
cmds.o: /usr/include/sys/socket.h /usr/include/sys/un.h
cmds.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/pwd.h
cmds.o: /usr/include/syslog.h /usr/include/signal.h /usr/include/sys/wait.h
cmds.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
cmds.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
cmds.o: /usr/include/ctype.h /usr/include/errno.h lp.local.h
cmds.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ar.h
cmds.o: /usr/include/sys/time.h /usr/include/time.h
cmdtab.o: cmdtab.c lpc.h
printjob.o: printjob.c lp.h /usr/include/stdio.h /usr/include/sys/param.h
printjob.o: /usr/include/sys/types.h /usr/include/signal.h
printjob.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
printjob.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
printjob.o: /usr/include/sys/socket.h /usr/include/sys/un.h
printjob.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/pwd.h
printjob.o: /usr/include/syslog.h /usr/include/signal.h /usr/include/sys/wait.h
printjob.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
printjob.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
printjob.o: /usr/include/ctype.h /usr/include/errno.h lp.local.h
printjob.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ar.h
recvjob.o: recvjob.c lp.h /usr/include/stdio.h /usr/include/sys/param.h
recvjob.o: /usr/include/sys/types.h /usr/include/signal.h
recvjob.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
recvjob.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
recvjob.o: /usr/include/sys/socket.h /usr/include/sys/un.h
recvjob.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/pwd.h
recvjob.o: /usr/include/syslog.h /usr/include/signal.h /usr/include/sys/wait.h
recvjob.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
recvjob.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
recvjob.o: /usr/include/ctype.h /usr/include/errno.h lp.local.h
recvjob.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ar.h
recvjob.o: /usr/include/sys/fs.h
displayq.o: displayq.c lp.h /usr/include/stdio.h /usr/include/sys/param.h
displayq.o: /usr/include/sys/types.h /usr/include/signal.h
displayq.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
displayq.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
displayq.o: /usr/include/sys/socket.h /usr/include/sys/un.h
displayq.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/pwd.h
displayq.o: /usr/include/syslog.h /usr/include/signal.h /usr/include/sys/wait.h
displayq.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
displayq.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
displayq.o: /usr/include/ctype.h /usr/include/errno.h lp.local.h
displayq.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ar.h
rmjob.o: rmjob.c lp.h /usr/include/stdio.h /usr/include/sys/param.h
rmjob.o: /usr/include/sys/types.h /usr/include/signal.h
rmjob.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
rmjob.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
rmjob.o: /usr/include/sys/socket.h /usr/include/sys/un.h
rmjob.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/pwd.h
rmjob.o: /usr/include/syslog.h /usr/include/signal.h /usr/include/sys/wait.h
rmjob.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
rmjob.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
rmjob.o: /usr/include/ctype.h /usr/include/errno.h lp.local.h
rmjob.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ar.h
startdaemon.o: startdaemon.c /usr/include/stdio.h /usr/include/sys/types.h
startdaemon.o: /usr/include/sys/socket.h /usr/include/sys/un.h lp.local.h
startdaemon.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ar.h
common.o: common.c lp.h /usr/include/stdio.h /usr/include/sys/param.h
common.o: /usr/include/sys/types.h /usr/include/signal.h
common.o: /usr/include/machine/machparam.h /usr/include/sys/file.h
common.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
common.o: /usr/include/sys/socket.h /usr/include/sys/un.h
common.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/pwd.h
common.o: /usr/include/syslog.h /usr/include/signal.h /usr/include/sys/wait.h
common.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
common.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
common.o: /usr/include/ctype.h /usr/include/errno.h lp.local.h
common.o: /usr/include/a.out.h /usr/include/sys/exec.h /usr/include/ar.h
printcap.o: printcap.c /usr/include/ctype.h /usr/include/stdio.h
lpdchar.o: lpdchar.c lp.local.h /usr/include/a.out.h /usr/include/sys/exec.h
lpdchar.o: /usr/include/ar.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
