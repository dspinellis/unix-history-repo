#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
MDIR=	/usr/man/cat4
SRCS=	arp.4 bk.4 drum.4 icmp.4 idp.4 imp.4 inet.4 ip.4 kdb.4 \
	lo.4 netintro.4 ns.4 nsip.4 null.4 pty.4 spp.4 tcp.4 tty.4 udp.4
OBJS=	arp.0 bk.0 drum.0 icmp.0 idp.0 imp.0 inet.0 ip.0 kdb.0 \
	lo.0 netintro.0 ns.0 nsip.0 null.0 pty.0 spp.0 tcp.0 tty.0 udp.0
LINKS=	networking.0

.SUFFIXES: .4 .0

.4.0:
	${DESTDIR}/usr/man/manroff $*.4 > $*.0

all: ${OBJS}

clean: FRC
	rm -f ${OBJS}

install: FRC
	cd ${DESTDIR}${MDIR}; rm -f *.0
	install -c -o bin -g bin -m 444 ${OBJS} ${DESTDIR}${MDIR}
	ln ${DESTDIR}${MDIR}/netintro.0 ${DESTDIR}${MDIR}/networking.0

FRC:
