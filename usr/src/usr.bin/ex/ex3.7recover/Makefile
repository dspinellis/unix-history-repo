#	@(#)Makefile	8.1 (Berkeley) 6/9/93

PROG=	ex3.7recover
SRCS=	ex3.7recover.c ex_extern.c
CFLAGS+=-I${.CURDIR}/.. -I${.CURDIR}/../obj -DVMUNIX
.PATH:	${.CURDIR}/..
BINOWN=	root
BINDIR=	/usr/libexec
BINMODE=4555
NOMAN=	noman

.include <bsd.prog.mk>
