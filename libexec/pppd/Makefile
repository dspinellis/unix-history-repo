#
#	$Id$
#
PROG=	pppd
MAN8=	pppd.8
SRCS=	main.c magic.c fsm.c lcp.c ipcp.c upap.c logwtmp.c chap.c md5.c
MD5SRCS = md5driver.c

DEBUG_FLAGS = -DDEBUGFSM -DDEBUGLCP -DDEBUGIPCP -DDEBUGUPAP -DDEBUGCHAP \
	-DDEBUGMAIN

CFLAGS=	-DNO_DRAND48 -DKVMLIB -DPPP ${DEBUG_FLAGS}

DPADD+=	${LIBUTIL}
LDADD+=	-lutil

BINOWN=		root
BINGRP=		daemon
BINMODE=	4555

md5driver: md5.h md5.o md5driver.o
	$(CC) $(CFLAGS) -o md5driver md5driver.o md5.o

.include <bsd.prog.mk>
