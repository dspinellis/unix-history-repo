#	@(#)Makefile.inc	5.1 (Berkeley) %G%

BINDIR?=	/usr/bin
BINOWN?=	uucp
BINGRP?=	daemon

ETC=    	${DESTDIR}/etc
LIBDIR=		${DESTDIR}/usr/lib/uucp
PUBDIR= 	${DESTDIR}/var/spool/uucppublic

AUDIT=		${SPOOL}/AUDIT
CORRUPT=	${SPOOL}/CORRUPT
LCK=		${SPOOL}/LCK
LOG=		${SPOOL}/LOG
SPOOL=  	${DESTDIR}/var/spool/uucp
STST=		${SPOOL}/STST
XQTDIR=		${SPOOL}/XTMP

LIBUU=		${.CURDIR}/../libuu/obj/libuu.a
LIBACU=		${.CURDIR}/../libacu/obj/libacu.a
CFLAGS+=	-DUSE_OLD_TTY
