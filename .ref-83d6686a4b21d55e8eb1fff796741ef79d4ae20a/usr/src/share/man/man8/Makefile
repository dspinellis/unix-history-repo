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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
MDIR=	${DESTDIR}/usr/man/cat8
OBJS=	adduser.0 intro.0 makedev.0 makekey.0 rc.0 sticky.0

all clean depend lint tags:

cleandir:
	rm -f ${OBJS}

install: ${OBJS}
	install -c -o bin -g bin -m 444 ${OBJS} ${MDIR}
	rm -f ${MDIR}/accton.0; ln ${MDIR}/sa.0 ${MDIR}/accton.0
	rm -f ${MDIR}/umount.0; ln ${MDIR}/mount.0 ${MDIR}/umount.0
	rm -f ${MDIR}/mkfs.0; ln ${MDIR}/newfs.0 ${MDIR}/mkfs.0
	rm -f ${MDIR}/quotaoff.0; ln ${MDIR}/quotaon.0 ${MDIR}/quotaoff.0
	rm -f ${MDIR}/uucpd.0; ln ${MDIR}/uucico.0 ${MDIR}/uucpd.0
	rm -f ${MDIR}/fasthalt.0; ln ${MDIR}/fastboot.0 ${MDIR}/fasthalt.0
