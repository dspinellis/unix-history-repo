#
# Copyright (c) 1987 Regents of the University of California.
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
#	@(#)Makefile	5.3 (Berkeley) %G%
#
MDIR=	${DESTDIR}/usr/man/cat5
OBJS=	L-devices.0 L-dialcodes.0 L.aliases.0 L.cmds.0 L.sys.0 USERFILE.0 \
	a.out.0 acct.0 ar.0 core.0 dbx.0 dir.0 disktab.0 dump.0 fs.0 \
	fstab.0 gettytab.0 group.0 hosts.0 mtab.0 networks.0 \
	passwd.0 phones.0 plot.0 printcap.0 protocols.0 remote.0 \
	resolver.0 services.0 shells.0 stab.0 tar.0 tp.0 ttys.0 types.0 \
	tzfile.0 utmp.0 uuencode.0 vfont.0 vgrindefs.0

all clean depend lint tags:

cleandir:
	rm -f ${OBJS}

install: ${OBJS}
	install -c -o bin -g bin -m 444 ${OBJS} ${MDIR}
	rm -f ${MDIR}/inode.0; ln ${MDIR}/fs.0 ${MDIR}/inode.0
	rm -f ${MDIR}/dumpdates.0; ln ${MDIR}/dump.0 ${MDIR}/dumpdates.0
	rm -f ${MDIR}/wtmp.0; ln ${MDIR}/utmp.0 ${MDIR}/wtmp.0
