#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	lpc
CFLAGS+=-I${.CURDIR}/../common_source
MAN8=	lpc.0
SRCS=	lpc.c cmds.c cmdtab.c startdaemon.c common.c printcap.c
.PATH:	${.CURDIR}/../common_source

.include "../../Makefile.inc"
.include <bsd.prog.mk>
