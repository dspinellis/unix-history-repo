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
#	@(#)Makefile	5.9 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	host.c inet.c if.c main.c mbuf.c route.c unix.c ns.c
OBJS=	host.o inet.o if.o main.o mbuf.o route.o unix.o ns.o bbnnet.o

all: netstat

netstat: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core netstat

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g kmem -m 2755 netstat ${DESTDIR}/usr/ucb/netstat

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

host.o: host.c /usr/include/sys/types.h /usr/include/sys/mbuf.h
host.o: /usr/include/netinet/in.h /usr/include/netimp/if_imp.h
host.o: /usr/include/netimp/if_imphost.h
inet.o: inet.c /usr/include/strings.h /usr/include/stdio.h
inet.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
inet.o: /usr/include/machine/machparam.h /usr/include/sys/socket.h
inet.o: /usr/include/sys/socketvar.h /usr/include/sys/mbuf.h
inet.o: /usr/include/sys/protosw.h /usr/include/net/route.h
inet.o: /usr/include/netinet/in.h /usr/include/netinet/in_systm.h
inet.o: /usr/include/netinet/in_pcb.h /usr/include/netinet/ip.h
inet.o: /usr/include/netinet/ip_icmp.h /usr/include/netinet/icmp_var.h
inet.o: /usr/include/netinet/ip_var.h /usr/include/netinet/tcp.h
inet.o: /usr/include/netinet/tcpip.h /usr/include/netinet/tcp_seq.h
inet.o: /usr/include/netinet/tcp_fsm.h /usr/include/netinet/tcp_timer.h
inet.o: /usr/include/netinet/tcp_var.h /usr/include/netinet/tcp_debug.h
inet.o: /usr/include/netinet/udp.h /usr/include/netinet/udp_var.h
inet.o: /usr/include/netdb.h
if.o: if.c /usr/include/sys/types.h /usr/include/sys/socket.h
if.o: /usr/include/net/if.h /usr/include/net/if_arp.h /usr/include/netinet/in.h
if.o: /usr/include/netinet/in_var.h /usr/include/netns/ns.h
if.o: /usr/include/stdio.h /usr/include/signal.h
main.o: main.c /usr/include/sys/param.h /usr/include/sys/types.h
main.o: /usr/include/signal.h /usr/include/machine/machparam.h
main.o: /usr/include/sys/vmmac.h /usr/include/sys/socket.h
main.o: /usr/include/machine/pte.h /usr/include/ctype.h /usr/include/errno.h
main.o: /usr/include/netdb.h /usr/include/nlist.h /usr/include/stdio.h
mbuf.o: mbuf.c /usr/include/stdio.h /usr/include/sys/param.h
mbuf.o: /usr/include/sys/types.h /usr/include/signal.h
mbuf.o: /usr/include/machine/machparam.h /usr/include/sys/mbuf.h
route.o: route.c /usr/include/stdio.h /usr/include/strings.h
route.o: /usr/include/sys/param.h /usr/include/sys/types.h
route.o: /usr/include/signal.h /usr/include/machine/machparam.h
route.o: /usr/include/sys/socket.h /usr/include/sys/mbuf.h
route.o: /usr/include/net/if.h /usr/include/net/if_arp.h
route.o: /usr/include/net/route.h /usr/include/netinet/in.h
route.o: /usr/include/netns/ns.h /usr/include/netdb.h
unix.o: unix.c /usr/include/sys/param.h /usr/include/sys/types.h
unix.o: /usr/include/signal.h /usr/include/machine/machparam.h
unix.o: /usr/include/sys/protosw.h /usr/include/sys/socket.h
unix.o: /usr/include/sys/socketvar.h /usr/include/sys/mbuf.h
unix.o: /usr/include/sys/un.h /usr/include/sys/unpcb.h /usr/include/sys/file.h
ns.o: ns.c /usr/include/stdio.h /usr/include/errno.h /usr/include/nlist.h
ns.o: /usr/include/sys/types.h /usr/include/sys/socket.h
ns.o: /usr/include/sys/socketvar.h /usr/include/sys/mbuf.h
ns.o: /usr/include/sys/protosw.h /usr/include/net/route.h /usr/include/net/if.h
ns.o: /usr/include/net/if_arp.h /usr/include/netinet/tcp_fsm.h
ns.o: /usr/include/netinet/tcp_timer.h /usr/include/netns/ns.h
ns.o: /usr/include/netns/ns_pcb.h /usr/include/netns/idp.h
ns.o: /usr/include/netns/idp_var.h /usr/include/netns/ns_error.h
ns.o: /usr/include/netns/sp.h /usr/include/netns/spidp.h
ns.o: /usr/include/netns/spp_var.h /usr/include/netns/spp_debug.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
