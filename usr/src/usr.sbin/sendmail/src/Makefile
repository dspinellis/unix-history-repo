#
# Copyright (c) 1988 Regents of the University of California.
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
#	@(#)Makefile	5.14 (Berkeley) %G%
#
SRCS=	alias.c arpadate.c clock.c collect.c conf.c convtime.c daemon.c \
	deliver.c domain.c envelope.c err.c headers.c macro.c main.c \
	parseaddr.c queue.c readcf.c recipient.c savemail.c srvrsmtp.c \
	stab.c stats.c sysexits.c trace.c usersmtp.c util.c Version.c
OBJS=	alias.o arpadate.o clock.o collect.o conf.o convtime.o daemon.o \
	deliver.o domain.o envelope.o err.o headers.o macro.o main.o \
	parseaddr.o queue.o readcf.o recipient.o savemail.o srvrsmtp.o \
	stab.o stats.o sysexits.o trace.o usersmtp.o util.o
MAN=	sendmail.0 newaliases.0 aliases.0

CFLAGS=	-O -I. -DVMUNIX

GET=	sccs get
DELTA=	sccs delta
WHAT=	what
PRT=	sccs prt

sendmail: ${OBJS} Version.o
	${CC} -o $@ Version.o ${OBJS} -ldbm
	size sendmail; ls -l sendmail; ${WHAT} < Version.o

dsendmail: ${OBJS} Version.o
	${CC} -g -o sendmail Version.o ${OBJS} -ldbm -lresolv
	size sendmail; ls -l sendmail; ${WHAT} < Version.o

install: ${MAN}
	install -c -s -o root -g kmem -m 6755 sendmail ${DESTDIR}/usr/sbin
	install -c -o bin -g bin -m 644 /dev/null ${DESTDIR}/etc/sendmail.fc
	install -c -o bin -g bin -m 666 /dev/null ${DESTDIR}/var/log/sendmail.st
	install -c -o bin -g bin -m 444 sendmail.hf /usr/share/misc
	install -c -o bin -g bin -m 444 sendmail.0 ${DESTDIR}/usr/man/cat8
	install -c -o bin -g bin -m 444 aliases.0 ${DESTDIR}/usr/man/cat5
	install -c -o bin -g bin -m 444 newaliases.0 ${DESTDIR}/usr/man/cat1
	-mkdir ${DESTDIR}/var/spool/mqueue
	chown root.wheel ${DESTDIR}/var/spool/mqueue
	chmod 775 ${DESTDIR}/var/spool/mqueue
	rm -f ${DESTDIR}/usr/bin/newaliases
	ln -s ${DESTDIR}/usr/sbin/sendmail ${DESTDIR}/usr/bin/newaliases
	rm -f ${DESTDIR}/usr/bin/mailq
	ln -s ${DESTDIR}/usr/sbin/sendmail ${DESTDIR}/usr/bin/mailq

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

clean: ${SRCS}
	rm -f ${OBJS} core sendmail a.out Version.o

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

lint: ${SRCS}
	lint -chapbx ${CCONFIG} ${SRCS}
