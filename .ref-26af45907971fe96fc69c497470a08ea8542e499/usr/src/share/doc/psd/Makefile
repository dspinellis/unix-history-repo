#	@(#)Makefile	5.2 (Berkeley) %G%

BINDIR=	/usr/share/doc/ps1
FILES=	00.contents Makefile Title
SUBDIR=	01.cacm 02.implement 03.iosys 04.uprog 05.sysman 06.Clang \
	07.pascal 08.f77 09.f77io 10.gdb 11.adb 12.make 13.rcs \
	14.sccs 15.yacc 16.lex 17.m4 18.gprof 19.curses 20.ipctut \
	21.ipc

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}

.include <bsd.subdir.mk>
