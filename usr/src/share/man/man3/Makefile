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
#	@(#)Makefile	5.13 (Berkeley) %G%
#
MDIR=	${DESTDIR}/usr/share/man/cat3
OBJS=	assert.0 bitstring.0 end.0 intro.0 monitor.0 varargs.0

all clean depend lint tags:

cleandir:
	rm -f ${OBJS}

install: ${OBJS}
	install -c -o bin -g bin -m 444 ${OBJS} ${MDIR}
	rm -f ${MDIR}/edata.0; ln ${MDIR}/end.0 ${MDIR}/edata.0
	rm -f ${MDIR}/etext.0; ln ${MDIR}/end.0 ${MDIR}/etext.0
	rm -f ${MDIR}/moncontrol.0; ln ${MDIR}/monitor.0 ${MDIR}/moncontrol.0
	rm -f ${MDIR}/monstartup.0; ln ${MDIR}/monitor.0 ${MDIR}/monstartup.0
