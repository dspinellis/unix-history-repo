#	Makefile	4.1	83/07/08
#
DESTDIR=
LESSONS=Init L0 L1.1a L1.1b L1.2 L2.1 L2.2 L2.3 L2.4 L3.1 \
	L3.2 L3.3 L3.4 L4.0 L4.1 L4.2 L4.3 L5.1 L5.2 L5.3 L5.4 \
	L6.1 L6.2 L6.3 L6.4 longtext

all:

install: ${LESSONS}
	-mkdir ${DESTDIR}/usr/lib/learn/vi
	chmod 755 ${DESTDIR}/usr/lib/learn/vi
	chown root ${DESTDIR}/usr/lib/learn/vi
	chgrp staff ${DESTDIR}/usr/lib/learn/vi
	tar cf - ${LESSONS} | (cd ${DESTDIR}/usr/lib/learn/vi; tar xpf -)

clean:
