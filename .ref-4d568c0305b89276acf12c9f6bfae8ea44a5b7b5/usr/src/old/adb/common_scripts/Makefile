#	@(#)Makefile	5.9 (Berkeley) %G%

LIST=	buf callout callout.next clist clist.nxt dino dir dir.nxt \
	dmap file filsys findinode findinode.nxt findproc host.nxt \
	hosts hosts.nxt hosttable idp ifaddr ifnet ifrw imp in_ifaddr \
	inode inpcb iovec ipreass ipreass.nxt mact mact.nxt mbstat \
	mbuf mbuf.nxt mbufchain mbufs mbufs.nxt mount nameidata \
	nspcb nsrtentry packetchain protosw quota rawcb route rtentry \
	rusage setproc setproc.done socket spp sppcb stat tcpcb tcpip \
	tcpreass tcpreass.nxt text traceall traceall.nxt tty termios \
	u u.all ubadev uio un unpcb urppcb vtimes why
NOOBJ=	noobj

all clean cleandir depend lint tags:

install:
	rm -f ${DESTDIR}/usr/old/libdata/adb/*
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${LIST} \
	    ${DESTDIR}/usr/old/libdata/adb

.include <bsd.prog.mk>
