#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
DESTDIR=
CFLAGS=	-O
OBJS=	main.o init.o subr.o gettytab.o get_date.o

all:	getty

getty:	${OBJS}
	${CC} -o getty ${OBJS}

install: all
	install -s getty ${DESTDIR}/etc/getty
	install -c -m 444 gettytab ${DESTDIR}/etc/gettytab

clean:
	rm -f getty ${OBJS} a.out core errs

depend:

main.o: gettytab.h
init.o: gettytab.h
subr.o: gettytab.h
