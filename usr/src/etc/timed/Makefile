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
#	@(#)Makefile	2.7 (Berkeley) 6/18/88
#
# optional flags are: MEASURE TESTING DEBUG
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCDS=	acksend.c candidate.c correct.c master.c networkdelta.c readmsg.c \
	slave.c timed.c
OBJDS=	acksend.o candidate.o correct.o master.o networkdelta.o readmsg.o \
	slave.o timed.o
SRCCS=	cmds.c cmdtab.c timedc.c
OBJCS=	cmds.o cmdtab.o timedc.o
COMMS=	byteorder.c measure.c cksum.${MACHINE}.c
COMMO=	byteorder.o measure.o cksum.${MACHINE}.o

all: timed timedc

timed: 	${OBJDS} ${COMMO} ${LIBC}
	${CC} ${CFLAGS} ${OBJDS} ${COMMO} ${LIBS} -o $@

timedc: ${OBJCS} ${COMMO} ${LIBC}
	${CC} ${CFLAGS} ${OBJCS} ${COMMO} ${LIBS} -o $@

cksum.${MACHINE}.o:
	${CC} ${CFLAGS} -c $*.c

clean: FRC
	rm -f ${OBJDS} ${OBJCS} ${COMMO} core timed timedc

depend: FRC
	mkdep ${CFLAGS} ${SRCDS} ${SRCCS} ${COMMS}

install: FRC
	install -s -o bin -g bin -m 755 timed ${DESTDIR}/etc/timed
	install -s -o root -g bin -m 4511 timedc ${DESTDIR}/etc/timedc

lint: FRC
	lint ${CFLAGS} ${SRCDS} ${COMMS}
	lint ${CFLAGS} ${SRCCS} ${COMMS}

tags: FRC
	ctags ${SRCDS} ${SRCCS} ${COMMS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

acksend.o: acksend.c globals.h /usr/include/sys/param.h
acksend.o: /usr/include/sys/types.h /usr/include/signal.h
acksend.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
acksend.o: /usr/include/machine/endian.h /usr/include/stdio.h
acksend.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
acksend.o: /usr/include/syslog.h /usr/include/sys/socket.h
acksend.o: /usr/include/netinet/in.h /usr/include/netdb.h
acksend.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
candidate.o: candidate.c globals.h /usr/include/sys/param.h
candidate.o: /usr/include/sys/types.h /usr/include/signal.h
candidate.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
candidate.o: /usr/include/machine/endian.h /usr/include/stdio.h
candidate.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
candidate.o: /usr/include/syslog.h /usr/include/sys/socket.h
candidate.o: /usr/include/netinet/in.h /usr/include/netdb.h
candidate.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
correct.o: correct.c globals.h /usr/include/sys/param.h
correct.o: /usr/include/sys/types.h /usr/include/signal.h
correct.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
correct.o: /usr/include/machine/endian.h /usr/include/stdio.h
correct.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
correct.o: /usr/include/syslog.h /usr/include/sys/socket.h
correct.o: /usr/include/netinet/in.h /usr/include/netdb.h
correct.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
master.o: master.c globals.h /usr/include/sys/param.h /usr/include/sys/types.h
master.o: /usr/include/signal.h /usr/include/machine/trap.h
master.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
master.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
master.o: /usr/include/errno.h /usr/include/syslog.h /usr/include/sys/socket.h
master.o: /usr/include/netinet/in.h /usr/include/netdb.h
master.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
master.o: /usr/include/sys/file.h /usr/include/setjmp.h /usr/include/utmp.h
networkdelta.o: networkdelta.c globals.h /usr/include/sys/param.h
networkdelta.o: /usr/include/sys/types.h /usr/include/signal.h
networkdelta.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
networkdelta.o: /usr/include/machine/endian.h /usr/include/stdio.h
networkdelta.o: /usr/include/sys/time.h /usr/include/time.h
networkdelta.o: /usr/include/errno.h /usr/include/syslog.h
networkdelta.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
networkdelta.o: /usr/include/netdb.h /usr/include/arpa/inet.h
networkdelta.o: /usr/include/protocols/timed.h
readmsg.o: readmsg.c globals.h /usr/include/sys/param.h
readmsg.o: /usr/include/sys/types.h /usr/include/signal.h
readmsg.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
readmsg.o: /usr/include/machine/endian.h /usr/include/stdio.h
readmsg.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
readmsg.o: /usr/include/syslog.h /usr/include/sys/socket.h
readmsg.o: /usr/include/netinet/in.h /usr/include/netdb.h
readmsg.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
slave.o: slave.c globals.h /usr/include/sys/param.h /usr/include/sys/types.h
slave.o: /usr/include/signal.h /usr/include/machine/trap.h
slave.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
slave.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
slave.o: /usr/include/errno.h /usr/include/syslog.h /usr/include/sys/socket.h
slave.o: /usr/include/netinet/in.h /usr/include/netdb.h
slave.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
slave.o: /usr/include/setjmp.h
timed.o: timed.c globals.h /usr/include/sys/param.h /usr/include/sys/types.h
timed.o: /usr/include/signal.h /usr/include/machine/trap.h
timed.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
timed.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
timed.o: /usr/include/errno.h /usr/include/syslog.h /usr/include/sys/socket.h
timed.o: /usr/include/netinet/in.h /usr/include/netdb.h
timed.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
timed.o: /usr/include/net/if.h /usr/include/net/if_arp.h
timed.o: /usr/include/sys/file.h /usr/include/sys/ioctl.h
timed.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
timed.o: /usr/include/setjmp.h
cmds.o: cmds.c timedc.h /usr/include/sys/param.h /usr/include/sys/types.h
cmds.o: /usr/include/signal.h /usr/include/machine/trap.h
cmds.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cmds.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
cmds.o: /usr/include/errno.h /usr/include/sys/socket.h
cmds.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/arpa/inet.h
cmds.o: /usr/include/netinet/in_systm.h /usr/include/netinet/ip.h
cmds.o: /usr/include/netinet/ip_icmp.h /usr/include/protocols/timed.h
cmds.o: /usr/include/sys/file.h
cmdtab.o: cmdtab.c timedc.h /usr/include/sys/param.h /usr/include/sys/types.h
cmdtab.o: /usr/include/signal.h /usr/include/machine/trap.h
cmdtab.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
cmdtab.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
cmdtab.o: /usr/include/errno.h /usr/include/sys/socket.h
cmdtab.o: /usr/include/netinet/in.h /usr/include/netdb.h
cmdtab.o: /usr/include/arpa/inet.h
timedc.o: timedc.c timedc.h /usr/include/sys/param.h /usr/include/sys/types.h
timedc.o: /usr/include/signal.h /usr/include/machine/trap.h
timedc.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
timedc.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
timedc.o: /usr/include/errno.h /usr/include/sys/socket.h
timedc.o: /usr/include/netinet/in.h /usr/include/netdb.h
timedc.o: /usr/include/arpa/inet.h /usr/include/signal.h /usr/include/ctype.h
timedc.o: /usr/include/setjmp.h /usr/include/syslog.h
byteorder.o: byteorder.c globals.h /usr/include/sys/param.h
byteorder.o: /usr/include/sys/types.h /usr/include/signal.h
byteorder.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
byteorder.o: /usr/include/machine/endian.h /usr/include/stdio.h
byteorder.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
byteorder.o: /usr/include/syslog.h /usr/include/sys/socket.h
byteorder.o: /usr/include/netinet/in.h /usr/include/netdb.h
byteorder.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
measure.o: measure.c globals.h /usr/include/sys/param.h
measure.o: /usr/include/sys/types.h /usr/include/signal.h
measure.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
measure.o: /usr/include/machine/endian.h /usr/include/stdio.h
measure.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
measure.o: /usr/include/syslog.h /usr/include/sys/socket.h
measure.o: /usr/include/netinet/in.h /usr/include/netdb.h
measure.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
measure.o: /usr/include/netinet/in_systm.h /usr/include/netinet/ip.h
measure.o: /usr/include/netinet/ip_icmp.h
cksum.tahoe.o: cksum.tahoe.c /usr/include/sys/types.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
