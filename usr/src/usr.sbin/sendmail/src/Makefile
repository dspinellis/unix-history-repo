#
# Copyright (c) 1988 Regents of the University of California.
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
SRCS=	alias.c arpadate.c clock.c collect.c conf.c convtime.c daemon.c \
	deliver.c domain.c envelope.c err.c headers.c macro.c main.c \
	parseaddr.c queue.c readcf.c recipient.c savemail.c srvrsmtp.c \
	stab.c stats.c sysexits.c trace.c usersmtp.c util.c Version.c
OBJS=	alias.o arpadate.o clock.o collect.o conf.o convtime.o daemon.o \
	deliver.o domain.o envelope.o err.o headers.o macro.o main.o \
	parseaddr.o queue.o readcf.o recipient.o savemail.o srvrsmtp.o \
	stab.o stats.o sysexits.o trace.o usersmtp.o util.o

CFLAGS=	-O -I. -I../include -DVMUNIX -DMXDOMAIN
ASMSED=	../include/asm.sed

GET=	sccs get
DELTA=	sccs delta
WHAT=	what
PRT=	sccs prt

.c.o:
	cc -S ${CFLAGS} $*.c
	sed -f ${ASMSED} $*.s | as -o $*.o
	rm -f $*.s

sendmail: ${OBJS} Version.o
	${CC} ${COPTS} -o $@ Version.o ${OBJS} -ldbm
	size sendmail; ls -l sendmail; ${WHAT} < Version.o

dsendmail: ${OBJS} Version.o
	${CC} ${COPTS} -o sendmail Version.o ${OBJS} -ldbm -lresolv
	size sendmail; ls -l sendmail; ${WHAT} < Version.o

install:
	install -c -s -o root -g kmem -m 6755 sendmail ${DESTDIR}/usr/lib
	install -c -o bin -g bin -m 644 /dev/null ${DESTDIR}/usr/lib/sendmail.fc
	install -c -o bin -g bin -m 666 /dev/null ${DESTDIR}/usr/lib/sendmail.st
	rm -f ${DESTDIR}/usr/ucb/newaliases
	ln -s ${DESTDIR}/usr/lib/sendmail ${DESTDIR}/usr/ucb/newaliases
	rm -f ${DESTDIR}/usr/ucb/mailq
	ln -s ${DESTDIR}/usr/lib/sendmail ${DESTDIR}/usr/ucb/mailq

version: newversion ${OBJS} Version.c

newversion:
	@rm -f SCCS/p.version.c version.c
	@${GET} -e SCCS/s.version.c
	@${DELTA} -s SCCS/s.version.c
	@${GET} -t -s SCCS/s.version.c

Version.c: version.c
	@echo generating Version.c from version.c
	@cp version.c Version.c
	@chmod 644 Version.c
	@echo "" >> Version.c
	@echo "# ifdef COMMENT" >> Version.c
	@${PRT} SCCS/s.version.c >> Version.c
	@echo "" >> Version.c
	@echo "code versions:" >> Version.c
	@echo "" >> Version.c
	@${WHAT} ${OBJS} >> Version.c
	@echo "" >> Version.c
	@echo "# endif COMMENT" >> Version.c

clean: FRC
	rm -f ${OBJS} core sendmail rmail usersmtp uucp a.out sendmail.cf

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

lint: FRC
	lint -chapbx ${CCONFIG} ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

alias.o: alias.c /usr/include/pwd.h /usr/include/sys/types.h
alias.o: /usr/include/sys/stat.h /usr/include/signal.h
alias.o: /usr/include/machine/trap.h /usr/include/errno.h sendmail.h
alias.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/setjmp.h conf.h
alias.o: ../include/useful.h /usr/include/sys/syslog.h
alias.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
alias.o: /usr/include/sysexits.h /usr/include/sys/file.h
arpadate.o: arpadate.c conf.h /usr/include/sys/time.h /usr/include/time.h
arpadate.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
arpadate.o: ../include/useful.h
clock.o: clock.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
clock.o: /usr/include/setjmp.h conf.h ../include/useful.h
clock.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
clock.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
clock.o: /usr/include/sysexits.h /usr/include/signal.h
clock.o: /usr/include/machine/trap.h
collect.o: collect.c /usr/include/errno.h sendmail.h /usr/include/stdio.h
collect.o: /usr/include/ctype.h /usr/include/setjmp.h conf.h
collect.o: ../include/useful.h /usr/include/sys/types.h
collect.o: /usr/include/sys/syslog.h /usr/include/sys/socket.h
collect.o: /usr/include/netinet/in.h /usr/include/sysexits.h
conf.o: conf.c /usr/include/pwd.h /usr/include/sys/ioctl.h
conf.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydefaults.h
conf.o: /usr/include/sys/ttydev.h sendmail.h /usr/include/stdio.h
conf.o: /usr/include/ctype.h /usr/include/setjmp.h conf.h ../include/useful.h
conf.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
conf.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
conf.o: /usr/include/sysexits.h /usr/include/sys/stat.h /usr/include/nlist.h
conf.o: /usr/include/sys/wait.h /usr/include/machine/endian.h
convtime.o: convtime.c /usr/include/ctype.h ../include/useful.h
convtime.o: /usr/include/sys/types.h
daemon.o: daemon.c /usr/include/errno.h sendmail.h /usr/include/stdio.h
daemon.o: /usr/include/ctype.h /usr/include/setjmp.h conf.h ../include/useful.h
daemon.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
daemon.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
daemon.o: /usr/include/sysexits.h /usr/include/netdb.h
daemon.o: /usr/include/sys/signal.h /usr/include/machine/trap.h
daemon.o: /usr/include/sys/wait.h /usr/include/machine/endian.h
daemon.o: /usr/include/sys/time.h /usr/include/time.h
daemon.o: /usr/include/sys/resource.h
deliver.o: deliver.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
deliver.o: /usr/include/setjmp.h conf.h ../include/useful.h
deliver.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
deliver.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
deliver.o: /usr/include/sysexits.h /usr/include/sys/signal.h
deliver.o: /usr/include/machine/trap.h /usr/include/sys/stat.h
deliver.o: /usr/include/netdb.h /usr/include/errno.h
deliver.o: /usr/include/arpa/nameser.h /usr/include/resolv.h
domain.o: domain.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
domain.o: /usr/include/setjmp.h conf.h ../include/useful.h
domain.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
domain.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
domain.o: /usr/include/sysexits.h /usr/include/sys/param.h
domain.o: /usr/include/sys/types.h /usr/include/signal.h
domain.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
domain.o: /usr/include/machine/endian.h /usr/include/arpa/nameser.h
domain.o: /usr/include/resolv.h /usr/include/netdb.h
envelope.o: envelope.c /usr/include/pwd.h /usr/include/sys/time.h
envelope.o: /usr/include/time.h sendmail.h /usr/include/stdio.h
envelope.o: /usr/include/ctype.h /usr/include/setjmp.h conf.h
envelope.o: ../include/useful.h /usr/include/sys/types.h
envelope.o: /usr/include/sys/syslog.h /usr/include/sys/socket.h
envelope.o: /usr/include/netinet/in.h /usr/include/sysexits.h
envelope.o: /usr/include/sys/stat.h
err.o: err.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
err.o: /usr/include/setjmp.h conf.h ../include/useful.h
err.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
err.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
err.o: /usr/include/sysexits.h /usr/include/errno.h /usr/include/netdb.h
headers.o: headers.c /usr/include/errno.h sendmail.h /usr/include/stdio.h
headers.o: /usr/include/ctype.h /usr/include/setjmp.h conf.h
headers.o: ../include/useful.h /usr/include/sys/types.h
headers.o: /usr/include/sys/syslog.h /usr/include/sys/socket.h
headers.o: /usr/include/netinet/in.h /usr/include/sysexits.h
macro.o: macro.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
macro.o: /usr/include/setjmp.h conf.h ../include/useful.h
macro.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
macro.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
macro.o: /usr/include/sysexits.h
main.o: main.c /usr/include/signal.h /usr/include/machine/trap.h
main.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
main.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydefaults.h
main.o: /usr/include/sys/ttydev.h sendmail.h /usr/include/stdio.h
main.o: /usr/include/ctype.h /usr/include/setjmp.h conf.h ../include/useful.h
main.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
main.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
main.o: /usr/include/sysexits.h /usr/include/arpa/nameser.h
main.o: /usr/include/resolv.h
parseaddr.o: parseaddr.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
parseaddr.o: /usr/include/setjmp.h conf.h ../include/useful.h
parseaddr.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
parseaddr.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
parseaddr.o: /usr/include/sysexits.h
queue.o: queue.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
queue.o: /usr/include/setjmp.h conf.h ../include/useful.h
queue.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
queue.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
queue.o: /usr/include/sysexits.h /usr/include/sys/stat.h /usr/include/sys/dir.h
queue.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/errno.h
readcf.o: readcf.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
readcf.o: /usr/include/setjmp.h conf.h ../include/useful.h
readcf.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
readcf.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
readcf.o: /usr/include/sysexits.h
recipient.o: recipient.c /usr/include/pwd.h sendmail.h /usr/include/stdio.h
recipient.o: /usr/include/ctype.h /usr/include/setjmp.h conf.h
recipient.o: ../include/useful.h /usr/include/sys/types.h
recipient.o: /usr/include/sys/syslog.h /usr/include/sys/socket.h
recipient.o: /usr/include/netinet/in.h /usr/include/sysexits.h
recipient.o: /usr/include/sys/stat.h
savemail.o: savemail.c /usr/include/pwd.h sendmail.h /usr/include/stdio.h
savemail.o: /usr/include/ctype.h /usr/include/setjmp.h conf.h
savemail.o: ../include/useful.h /usr/include/sys/types.h
savemail.o: /usr/include/sys/syslog.h /usr/include/sys/socket.h
savemail.o: /usr/include/netinet/in.h /usr/include/sysexits.h
srvrsmtp.o: srvrsmtp.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
srvrsmtp.o: /usr/include/setjmp.h conf.h ../include/useful.h
srvrsmtp.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
srvrsmtp.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
srvrsmtp.o: /usr/include/sysexits.h /usr/include/errno.h /usr/include/signal.h
srvrsmtp.o: /usr/include/machine/trap.h
stab.o: stab.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
stab.o: /usr/include/setjmp.h conf.h ../include/useful.h
stab.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
stab.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
stab.o: /usr/include/sysexits.h
stats.o: stats.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
stats.o: /usr/include/setjmp.h conf.h ../include/useful.h
stats.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
stats.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
stats.o: /usr/include/sysexits.h mailstats.h
sysexits.o: sysexits.c /usr/include/sysexits.h
trace.o: trace.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
trace.o: /usr/include/setjmp.h conf.h ../include/useful.h
trace.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
trace.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
trace.o: /usr/include/sysexits.h
usersmtp.o: usersmtp.c sendmail.h /usr/include/stdio.h /usr/include/ctype.h
usersmtp.o: /usr/include/setjmp.h conf.h ../include/useful.h
usersmtp.o: /usr/include/sys/types.h /usr/include/sys/syslog.h
usersmtp.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
usersmtp.o: /usr/include/sysexits.h /usr/include/sysexits.h
usersmtp.o: /usr/include/errno.h
util.o: util.c /usr/include/stdio.h /usr/include/sys/types.h
util.o: /usr/include/sys/stat.h /usr/include/sysexits.h /usr/include/errno.h
util.o: sendmail.h /usr/include/stdio.h /usr/include/ctype.h
util.o: /usr/include/setjmp.h conf.h ../include/useful.h
util.o: /usr/include/sys/syslog.h /usr/include/sys/socket.h
util.o: /usr/include/netinet/in.h /usr/include/sysexits.h
Version.o: Version.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
