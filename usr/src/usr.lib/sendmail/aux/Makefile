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
#	@(#)Makefile	5.4 (Berkeley) 7/10/88
#
CFLAGS=	-O -I../src -I../include -DVMUNIX
ALL=	mconnect mailstats praliases mail rmail
SRCS=	mconnect.c mail.c mailstats.c praliases.c rmail.c
OBJS=	mconnect.o mail.o mailstats.o praliases.o rmail.o

.c.o:
	${CC} -S ${CFLAGS} $*.c
	sed -f ../include/asm.sed.${MACHINE} $*.s | as -o $*.o
	rm -f $*.s

all: ${ALL}

mail: mail.o
	${CC} -o $@ mail.o

rmail: rmail.o
	${CC} -o $@ rmail.o

mconnect: mconnect.o
	${CC} -o $@ mconnect.o

mailstats: mailstats.o
	${CC} -o $@ mailstats.o

praliases: praliases.o
	${CC} -o $@ praliases.o -ldbm

clean: FRC
	rm -f core ${ALL} ${OBJS}

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o root -g bin -m 4755 mail ${DESTDIR}/bin/mail
	install -s -o bin -g bin -m 755 rmail ${DESTDIR}/bin/rmail
	install -s -o bin -g bin -m 755 mconnect mailstats praliases ${DESTDIR}/usr/lib

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

mconnect.o: mconnect.c /usr/include/stdio.h /usr/include/signal.h
mconnect.o: /usr/include/machine/trap.h /usr/include/ctype.h
mconnect.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
mconnect.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mconnect.o: /usr/include/sys/types.h /usr/include/sys/socket.h
mconnect.o: /usr/include/netinet/in.h /usr/include/netdb.h
mail.o: mail.c /usr/include/sys/param.h /usr/include/sys/types.h
mail.o: /usr/include/signal.h /usr/include/machine/trap.h
mail.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
mail.o: /usr/include/sys/stat.h /usr/include/sys/file.h /usr/include/ctype.h
mail.o: /usr/include/stdio.h /usr/include/pwd.h /usr/include/utmp.h
mail.o: /usr/include/signal.h /usr/include/setjmp.h /usr/include/sysexits.h
mail.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
mail.o: /usr/include/netdb.h
mailstats.o: mailstats.c /usr/include/sys/file.h ../src/sendmail.h
mailstats.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/setjmp.h
mailstats.o: ../src/conf.h ../include/useful.h /usr/include/sys/types.h
mailstats.o: /usr/include/sys/syslog.h /usr/include/sys/socket.h
mailstats.o: /usr/include/netinet/in.h /usr/include/sysexits.h
mailstats.o: ../src/mailstats.h
praliases.o: praliases.c ../src/sendmail.h /usr/include/stdio.h
praliases.o: /usr/include/ctype.h /usr/include/setjmp.h ../src/conf.h
praliases.o: ../include/useful.h /usr/include/sys/types.h
praliases.o: /usr/include/sys/syslog.h /usr/include/sys/socket.h
praliases.o: /usr/include/netinet/in.h /usr/include/sysexits.h
rmail.o: rmail.c /usr/include/stdio.h /usr/include/sysexits.h
rmail.o: /usr/include/sys/types.h /usr/include/sys/file.h
rmail.o: /usr/include/sys/stat.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
