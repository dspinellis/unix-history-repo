#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.3	(Berkeley)	%G%
#
LESSONS=C editor eqn files macros morefiles
FILES=	Linfo Xinfo

all:

install: FRC
	-mkdir ${DESTDIR}/usr/lib/learn ${DESTDIR}/usr/lib/learn/log \
		${DESTDIR}/usr/lib/learn/bin > /dev/null 2>&1
	chmod 755 ${DESTDIR}/usr/lib/learn ${DESTDIR}/usr/lib/learn/log \
		${DESTDIR}/usr/lib/learn/bin
	chown root.staff ${DESTDIR}/usr/lib/learn ${DESTDIR}/usr/lib/learn/log \
		${DESTDIR}/usr/lib/learn/bin
	install -c -o bin -g bin -m 644 Linfo ${DESTDIR}/usr/lib/learn/Linfo
	install -c -o bin -g bin -m 644 Xinfo ${DESTDIR}/usr/lib/learn/Xinfo
	tar cf - ${LESSONS} | (cd ${DESTDIR}/usr/lib/learn; tar xpf -);

clean:
depend:
lint:
tags:
FRC:
