#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific written prior permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.5 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
C2=	/lib/c2
INLINE=	/sys/machine/inline/inline
SRCS=	af.c if.c input.c main.c output.c startup.c tables.c timer.c \
	trace.c inet.c
OBJS=	af.o if.o input.o main.o output.o startup.o tables.o timer.o \
	trace.o inet.o

all: routed

routed:	${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

# The following can be deleted where not appropriate to use the kernel's
# inline code expansions.
.c.o:
	${CC} -S ${CFLAGS} $*.c
	${C2} $*.s | ${INLINE} | ${AS} -o $*.o
	rm -f $*.s

clean: FRC
	rm -f ${OBJS} core routed tags
	(cd tools; make ${MFLAGS} clean)

depend: FRC
	mkdep ${CFLAGS} ${SRCS}
	(cd tools; make ${MFLAGS} depend)

install: FRC
	install -s -o bin -g bin -m 755 routed ${DESTDIR}/etc/routed

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}
	(cd tools; make ${MFLAGS} tags)

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

af.o: af.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
af.o: /usr/include/net/route.h /usr/include/netinet/in.h
af.o: /usr/include/protocols/routed.h /usr/include/stdio.h /usr/include/netdb.h
af.o: trace.h interface.h table.h af.h
if.o: if.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
if.o: /usr/include/net/route.h /usr/include/netinet/in.h
if.o: /usr/include/protocols/routed.h /usr/include/stdio.h /usr/include/netdb.h
if.o: trace.h interface.h table.h af.h
input.o: input.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
input.o: /usr/include/net/route.h /usr/include/netinet/in.h
input.o: /usr/include/protocols/routed.h /usr/include/stdio.h
input.o: /usr/include/netdb.h trace.h interface.h table.h af.h
input.o: /usr/include/sys/syslog.h
main.o: main.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
main.o: /usr/include/net/route.h /usr/include/netinet/in.h
main.o: /usr/include/protocols/routed.h /usr/include/stdio.h
main.o: /usr/include/netdb.h trace.h interface.h table.h af.h
main.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
main.o: /usr/include/sys/ttydev.h /usr/include/sys/time.h /usr/include/time.h
main.o: /usr/include/net/if.h /usr/include/net/if_arp.h /usr/include/errno.h
main.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/syslog.h
output.o: output.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
output.o: /usr/include/net/route.h /usr/include/netinet/in.h
output.o: /usr/include/protocols/routed.h /usr/include/stdio.h
output.o: /usr/include/netdb.h trace.h interface.h table.h af.h
startup.o: startup.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
startup.o: /usr/include/net/route.h /usr/include/netinet/in.h
startup.o: /usr/include/protocols/routed.h /usr/include/stdio.h
startup.o: /usr/include/netdb.h trace.h interface.h table.h af.h
startup.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
startup.o: /usr/include/sys/ttydev.h /usr/include/net/if.h
startup.o: /usr/include/net/if_arp.h /usr/include/syslog.h
tables.o: tables.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
tables.o: /usr/include/net/route.h /usr/include/netinet/in.h
tables.o: /usr/include/protocols/routed.h /usr/include/stdio.h
tables.o: /usr/include/netdb.h trace.h interface.h table.h af.h
tables.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
tables.o: /usr/include/sys/ttydev.h /usr/include/errno.h /usr/include/syslog.h
timer.o: timer.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
timer.o: /usr/include/net/route.h /usr/include/netinet/in.h
timer.o: /usr/include/protocols/routed.h /usr/include/stdio.h
timer.o: /usr/include/netdb.h trace.h interface.h table.h af.h
trace.o: trace.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
trace.o: /usr/include/net/route.h /usr/include/netinet/in.h
trace.o: /usr/include/protocols/routed.h /usr/include/stdio.h
trace.o: /usr/include/netdb.h trace.h interface.h table.h af.h
trace.o: /usr/include/sys/stat.h
inet.o: inet.c defs.h /usr/include/sys/types.h /usr/include/sys/socket.h
inet.o: /usr/include/net/route.h /usr/include/netinet/in.h
inet.o: /usr/include/protocols/routed.h /usr/include/stdio.h
inet.o: /usr/include/netdb.h trace.h interface.h table.h af.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
