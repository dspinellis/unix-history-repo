#	@(#)Makefile	5.11 (Berkeley) %G%

PROG=	tftpd
SRCS=	tftpd.c tftpsubs.c
MAN8=	tftpd.0
CFLAGS+=-I${.CURDIR}/../../usr.bin/tftp
.PATH:	${.CURDIR}/../../usr.bin/tftp

.include <bsd.prog.mk>
