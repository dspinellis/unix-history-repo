#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific written prior permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
LIST=	buf callout callout.next clist clist.nxt dino dir dir.nxt \
	dmap file filsys findinode findinode.nxt findproc host.nxt hosts \
	hosts.nxt hosttable idp ifaddr ifnet ifrw imp in_ifaddr inode inpcb \
	iovec ipreass ipreass.nxt mact mact.nxt mbstat mbuf mbuf.nxt \
	mbufchain mbufs mbufs.nxt mount nameidata nspcb nsrtentry packetchain \
	protosw quota rawcb route rtentry rusage setproc setproc.done socket \
	spp sppcb stat tcpcb tcpip tcpreass tcpreass.nxt text traceall \
	traceall.nxt u u.all ubadev uio un unpcb urppcb vtimes

all:

install: FRC
	install -c -o bin -g bin -m 444 ${LIST} ${DESTDIR}/usr/lib/adb

clean:
depend:
lint:
tags:
FRC:

# DO NOT DELETE THIS LINE -- make depend uses it

