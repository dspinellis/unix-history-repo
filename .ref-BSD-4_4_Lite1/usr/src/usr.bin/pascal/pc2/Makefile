#	@(#)Makefile	8.1 (Berkeley) 6/6/93

PROG=	pc2
CFLAGS=	-I/sys/${MACHINE}/inline
SRCS=	langpats.c libcpats.c machdep.c machpats.c main.c
.PATH:	/sys/${MACHINE}/inline
BINDIR=	/usr/libexec/pascal
NOMAN=	noman

.include <bsd.prog.mk>
