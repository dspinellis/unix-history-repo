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
#	@(#)Makefile	5.5 (Berkeley) 7/9/88
#

CFLAGS=	-O
HELPDIR=/usr/lib/help

all: help

help: help.c
	${CC} ${CFLAGS} -o $@ help.c

install:
	-mkdir ${HELPDIR} ${HELPDIR}/cat
	tar cf - maint src | (cd ${HELPDIR}; tar xf -)
	./help -d ${HELPDIR}/cat -m new .
	install -s -o bin -g bin -m 755 help ${DESTDIR}/usr/new/help
	/usr/man/manroff help.1 > help.0
	install -c -m 444 help.0 ${DESTDIR}/usr/new/man/cat1/help.0

macros:
	install -c -m 444 tmac.ayday ${DESTDIR}/usr/lib/tmac/tmac.ayday

clean:
	rm -f help help.0 core a.out

depend:
	mkdep -p ${CFLAGS} help.c

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

help: help.c /usr/include/sys/types.h /usr/include/sys/stat.h
help: /usr/include/sys/dir.h /usr/include/setjmp.h /usr/include/signal.h
help: /usr/include/machine/trap.h /usr/include/ctype.h /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
