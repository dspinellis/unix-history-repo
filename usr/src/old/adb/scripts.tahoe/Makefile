#	@(#)Makefile	5.7 (Berkeley) 4/26/93

LIST=	dirblk findproc.nxt frame pcb proc setproc.nxt trapframe
NOOBJ=	noobj

all clean cleandir depend lint tags:

install:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${LIST} \
	    ${DESTDIR}/usr/old/libdata/adb

.include <bsd.prog.mk>
