#	@(#)Makefile	5.10 (Berkeley) %G%

LIB=	dbm
CFLAGS+=-I${.CURDIR}
SRCS=	dbm.c ndbm.c
MAN3=	dbm.0 ndbm.0
MLINKS+=dbm.3 dbminit.3 dbm.3 delete.3 dbm.3 fetch.3 dbm.3 firstkey.3 \
	dbm.3 nextkey.3 dbm.3 store.3
MLINKS+=ndbm.3 dbm_clearerr.3 ndbm.3 dbm_close.3 ndbm.3 dbm_delete.3 \
	ndbm.3 dbm_error.3 ndbm.3 dbm_fetch.3 ndbm.3 dbm_firstkey.3 \
	ndbm.3 dbm_nextkey.3 ndbm.3 dbm_open.3 ndbm.3 dbm_store.3

beforeinstall:
	install -c -o ${BINOWN} -g ${BINGRP} -m ${MANMODE} ${.CURDIR}/dbm.h \
	    ${DESTDIR}/usr/include/dbm.h

.include <bsd.lib.mk>
