#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	timedc
SRCS=	cmds.c cmdtab.c timedc.c byteorder.c measure.c cksum.${MACHINE}.c
MAN8=	timedc.0
BINOWN=	root
BINMODE=4555
.PATH:	${.CURDIR}/../timed

.include <bsd.prog.mk>
