#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3 (Berkeley) %G%
#
ALL=	ftpd.o ftpcmd.o glob.o vers.o
DESTDIR=
CFLAGS=	-O

ftpd:	${ALL}
	${CC} -o ftpd ${ALL}

vers.o:	ftpd.c ftpcmd.y
	sh newvers.sh
	${CC} -c vers.c

install:
	install -s ftpd ${DESTDIR}/etc/ftpd

clean:
	rm -f ftpd *.o *.s errs core a.out t.? yacc.* y.tab.c

depend:
