#	@(#)Makefile	8.1 (Berkeley) 6/11/93

PROG=	arp
SRCS=	arp.c
CFLAGS+=-DLETS_GET_SMALL
BINGRP=	kmem
BINMODE=2555
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../usr.sbin/arp

.include <bsd.prog.mk>
