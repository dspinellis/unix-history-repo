#	@(#)Makefile	5.1 (Berkeley) 5/3/91

# USERMEM should be the amount of available user memory in bytes.
# Set it to zero, for physical memory less than 1 Meg.
USERMEM=4194304

PROG=	compress
SRCS=compress.c
CFLAGS+=-DLETS_GET_SMALL
NOMAN=	noman
LDFLAGS=-s -N
.PATH:	${.CURDIR}/../../../../usr.bin/compress

.include <bsd.prog.mk>
