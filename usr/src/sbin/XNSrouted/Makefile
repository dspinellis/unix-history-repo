#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.7	(Berkeley)	%G%
#
#	Makefile for Xerox Routing Information Protocol
#	Daemon
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	af.c if.c input.c main.c output.c startup.c tables.c timer.c \
	trace.c
OBJS=	af.o if.o input.o main.o output.o startup.o tables.o timer.o \
	trace.o

all: XNSrouted

XNSrouted: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core XNSrouted

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o bin -g bin -m 755 XNSrouted ${DESTDIR}/etc/XNSrouted

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

af.o: af.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
af.o: /usr/include/net/route.h /usr/include/netns/ns.h /usr/include/netns/idp.h
af.o: /usr/include/stdio.h /usr/include/syslog.h protocol.h trace.h interface.h
af.o: table.h af.h
if.o: if.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
if.o: /usr/include/net/route.h /usr/include/netns/ns.h /usr/include/netns/idp.h
if.o: /usr/include/stdio.h /usr/include/syslog.h protocol.h trace.h interface.h
if.o: table.h af.h
input.o: input.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
input.o: /usr/include/net/route.h /usr/include/netns/ns.h
input.o: /usr/include/netns/idp.h /usr/include/stdio.h /usr/include/syslog.h
input.o: protocol.h trace.h interface.h table.h af.h
main.o: main.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
main.o: /usr/include/net/route.h /usr/include/netns/ns.h
main.o: /usr/include/netns/idp.h /usr/include/stdio.h /usr/include/syslog.h
main.o: protocol.h trace.h interface.h table.h af.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
main.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/net/if.h
main.o: /usr/include/net/if_arp.h /usr/include/errno.h /usr/include/nlist.h
main.o: /usr/include/signal.h
output.o: output.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
output.o: /usr/include/net/route.h /usr/include/netns/ns.h
output.o: /usr/include/netns/idp.h /usr/include/stdio.h /usr/include/syslog.h
output.o: protocol.h trace.h interface.h table.h af.h
startup.o: startup.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
startup.o: /usr/include/net/route.h /usr/include/netns/ns.h
startup.o: /usr/include/netns/idp.h /usr/include/stdio.h /usr/include/syslog.h
startup.o: protocol.h trace.h interface.h table.h af.h /usr/include/sys/ioctl.h
startup.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
startup.o: /usr/include/net/if.h /usr/include/net/if_arp.h /usr/include/nlist.h
startup.o: /usr/include/syslog.h
tables.o: tables.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
tables.o: /usr/include/net/route.h /usr/include/netns/ns.h
tables.o: /usr/include/netns/idp.h /usr/include/stdio.h /usr/include/syslog.h
tables.o: protocol.h trace.h interface.h table.h af.h /usr/include/sys/ioctl.h
tables.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tables.o: /usr/include/errno.h
timer.o: timer.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
timer.o: /usr/include/net/route.h /usr/include/netns/ns.h
timer.o: /usr/include/netns/idp.h /usr/include/stdio.h /usr/include/syslog.h
timer.o: protocol.h trace.h interface.h table.h af.h
trace.o: trace.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
trace.o: /usr/include/net/route.h /usr/include/netns/ns.h
trace.o: /usr/include/netns/idp.h /usr/include/stdio.h /usr/include/syslog.h
trace.o: protocol.h trace.h interface.h table.h af.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
