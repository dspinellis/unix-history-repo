#	@(#)Makefile	5.1 (Berkeley) %G%

PROG=	ftp
SRCS=	cmds.c cmdtab.c ftp.c glob.c main.c ruserpass.c domacro.c
CFLAGS+=-DLETS_GET_SMALL
NOMAN=	noman
LDFLAGS=-N
.PATH:	${.CURDIR}/../../../../usr.bin/ftp

.include <bsd.prog.mk>
