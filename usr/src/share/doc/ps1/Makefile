#	@(#)Makefile	5.1 (Berkeley) 7/1/90

BINDIR=	/usr/share/doc/ps1
FILES=	00.contents Makefile Title
SUBDIR=	01.Clang 02.f77 03.f77io 04.pascal 05.as 06.sysman 07.ipctut \
	08.ipc 09.lint 10.adb 11.dbx 12.make 13.rcs 14.sccs 15.yacc \
	16.lex 17.m4 18.curses

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} \
	    ${DESTDIR}${BINDIR}

.include <bsd.subdir.mk>
