#	@(#)Makefile	5.9 (Berkeley) %G%

LIB=	dbm
SRCS=	dbm.c
MAN3=	dbm.0
MLINKS+=dbm.3 dbminit.3 dbm.3 delete.3 dbm.3 fetch.3 dbm.3 firstkey.3 \
	dbm.3 nextkey.3 dbm.3 store.3

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${.CURDIR}/dbm.h \
	    ${DESTDIR}/usr/include/dbm.h

.include <bsd.lib.mk>
