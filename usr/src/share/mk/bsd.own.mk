#	@(#)bsd.own.mk	5.1 (Berkeley) %G%

.if exists(${.CURDIR}/../Makefile.inc)
.include "${.CURDIR}/../Makefile.inc"
.endif

BINGRP?=	bin
BINOWN?=	bin
BINMODE?=	555

STRIP?=		-s

MANGRP?=	bin
MANOWN?=	bin
MANMODE?=	444

MANDIR?=	/usr/share/man/cat
