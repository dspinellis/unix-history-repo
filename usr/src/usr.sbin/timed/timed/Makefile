#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1	(Berkeley)	%G%
#
# optional flags are: MEASURE TESTING DEBUG

CFLAGS= -O
DESTDIR=
LIBS=
PROGRS=	timed timedc

SRCDS=	acksend.c candidate.c correct.c \
	master.c networkdelta.c readmsg.c slave.c timed.c

OBJDS=	acksend.o candidate.o correct.o \
	master.o networkdelta.o readmsg.o slave.o timed.o

SRCCS=	cmds.c cmdtab.c timedc.c

OBJCS=	cmds.o cmdtab.o timedc.o

COMMS=	byteorder.c measure.c cksum.${MACHINE}.c

COMMO=	byteorder.o measure.o cksum.${MACHINE}.o

all:	${PROGRS}

cksum.${MACHINE}.o:
	cc ${CFLAGS} -c $*.c

timed: 	${OBJDS} ${COMMO}
	cc ${CFLAGS} ${OBJDS} ${COMMO} ${LIBS} -o timed 

timedc: ${OBJCS} ${COMMO}
	cc ${CFLAGS} ${OBJCS} ${COMMO} ${LIBS} -o timedc

install: ${PROGRS}
	install -s timed ${DESTDIR}/etc
	install -s -o root -m 4511 timedc ${DESTDIR}/etc

clean:
	rm -f a.out core *.o Makefile.bak errs ${PROGRS}

lint:
	lint ${SRCDS} ${COMMS}
	lint ${SRCCS} ${COMMS}

depend:
	for i in ${SRCCS} ${SRCDS} ${COMMS}; do \
	    cc -M ${CFLAGS} $$i | awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

cmds.o: cmds.c ./timedc.h /usr/include/sys/param.h /usr/include/sys/types.h
cmds.o: /usr/include/signal.h /usr/include/machine/machparam.h
cmds.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
cmds.o: /usr/include/errno.h /usr/include/sys/socket.h
cmds.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/arpa/inet.h
cmds.o: /usr/include/netinet/in_systm.h /usr/include/netinet/ip.h
cmds.o: /usr/include/netinet/ip_icmp.h /usr/include/protocols/timed.h
cmds.o: /usr/include/sys/file.h
cmdtab.o: cmdtab.c ./timedc.h /usr/include/sys/param.h /usr/include/sys/types.h
cmdtab.o: /usr/include/signal.h /usr/include/machine/machparam.h
cmdtab.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
cmdtab.o: /usr/include/errno.h /usr/include/sys/socket.h
cmdtab.o: /usr/include/netinet/in.h /usr/include/netdb.h
cmdtab.o: /usr/include/arpa/inet.h
timedc.o: timedc.c ./timedc.h /usr/include/sys/param.h /usr/include/sys/types.h
timedc.o: /usr/include/signal.h /usr/include/machine/machparam.h
timedc.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
timedc.o: /usr/include/errno.h /usr/include/sys/socket.h
timedc.o: /usr/include/netinet/in.h /usr/include/netdb.h
timedc.o: /usr/include/arpa/inet.h /usr/include/signal.h /usr/include/ctype.h
timedc.o: /usr/include/setjmp.h /usr/include/syslog.h
acksend.o: acksend.c ./globals.h /usr/include/sys/param.h
acksend.o: /usr/include/sys/types.h /usr/include/signal.h
acksend.o: /usr/include/machine/machparam.h /usr/include/stdio.h
acksend.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
acksend.o: /usr/include/syslog.h /usr/include/sys/socket.h
acksend.o: /usr/include/netinet/in.h /usr/include/netdb.h
acksend.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
candidate.o: candidate.c ./globals.h /usr/include/sys/param.h
candidate.o: /usr/include/sys/types.h /usr/include/signal.h
candidate.o: /usr/include/machine/machparam.h /usr/include/stdio.h
candidate.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
candidate.o: /usr/include/syslog.h /usr/include/sys/socket.h
candidate.o: /usr/include/netinet/in.h /usr/include/netdb.h
candidate.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
correct.o: correct.c ./globals.h /usr/include/sys/param.h
correct.o: /usr/include/sys/types.h /usr/include/signal.h
correct.o: /usr/include/machine/machparam.h /usr/include/stdio.h
correct.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
correct.o: /usr/include/syslog.h /usr/include/sys/socket.h
correct.o: /usr/include/netinet/in.h /usr/include/netdb.h
correct.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
master.o: master.c ./globals.h /usr/include/sys/param.h
master.o: /usr/include/sys/types.h /usr/include/signal.h
master.o: /usr/include/machine/machparam.h /usr/include/stdio.h
master.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
master.o: /usr/include/syslog.h /usr/include/sys/socket.h
master.o: /usr/include/netinet/in.h /usr/include/netdb.h
master.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
master.o: /usr/include/sys/file.h /usr/include/setjmp.h /usr/include/utmp.h
networkdelta.o: networkdelta.c ./globals.h /usr/include/sys/param.h
networkdelta.o: /usr/include/sys/types.h /usr/include/signal.h
networkdelta.o: /usr/include/machine/machparam.h /usr/include/stdio.h
networkdelta.o: /usr/include/sys/time.h /usr/include/time.h
networkdelta.o: /usr/include/errno.h /usr/include/syslog.h
networkdelta.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
networkdelta.o: /usr/include/netdb.h /usr/include/arpa/inet.h
networkdelta.o: /usr/include/protocols/timed.h
readmsg.o: readmsg.c ./globals.h /usr/include/sys/param.h
readmsg.o: /usr/include/sys/types.h /usr/include/signal.h
readmsg.o: /usr/include/machine/machparam.h /usr/include/stdio.h
readmsg.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
readmsg.o: /usr/include/syslog.h /usr/include/sys/socket.h
readmsg.o: /usr/include/netinet/in.h /usr/include/netdb.h
readmsg.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
slave.o: slave.c ./globals.h /usr/include/sys/param.h /usr/include/sys/types.h
slave.o: /usr/include/signal.h /usr/include/machine/machparam.h
slave.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
slave.o: /usr/include/errno.h /usr/include/syslog.h /usr/include/sys/socket.h
slave.o: /usr/include/netinet/in.h /usr/include/netdb.h
slave.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
slave.o: /usr/include/setjmp.h
timed.o: timed.c ./globals.h /usr/include/sys/param.h /usr/include/sys/types.h
timed.o: /usr/include/signal.h /usr/include/machine/machparam.h
timed.o: /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
timed.o: /usr/include/errno.h /usr/include/syslog.h /usr/include/sys/socket.h
timed.o: /usr/include/netinet/in.h /usr/include/netdb.h
timed.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
timed.o: /usr/include/net/if.h /usr/include/net/if_arp.h
timed.o: /usr/include/sys/file.h /usr/include/sys/ioctl.h
timed.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
timed.o: /usr/include/setjmp.h
byteorder.o: byteorder.c ./globals.h /usr/include/sys/param.h
byteorder.o: /usr/include/sys/types.h /usr/include/signal.h
byteorder.o: /usr/include/machine/machparam.h /usr/include/stdio.h
byteorder.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
byteorder.o: /usr/include/syslog.h /usr/include/sys/socket.h
byteorder.o: /usr/include/netinet/in.h /usr/include/netdb.h
byteorder.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
measure.o: measure.c ./globals.h /usr/include/sys/param.h
measure.o: /usr/include/sys/types.h /usr/include/signal.h
measure.o: /usr/include/machine/machparam.h /usr/include/stdio.h
measure.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
measure.o: /usr/include/syslog.h /usr/include/sys/socket.h
measure.o: /usr/include/netinet/in.h /usr/include/netdb.h
measure.o: /usr/include/arpa/inet.h /usr/include/protocols/timed.h
measure.o: /usr/include/netinet/in_systm.h /usr/include/netinet/ip.h
measure.o: /usr/include/netinet/ip_icmp.h
cksum.tahoe.o: cksum.tahoe.c /usr/include/sys/types.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
