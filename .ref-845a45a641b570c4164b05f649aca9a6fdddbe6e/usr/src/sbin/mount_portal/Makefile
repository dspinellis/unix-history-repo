#	@(#)Makefile	8.1 (Berkeley) %G%

CFLAGS+=-I/sys
PROG=	mount_portal
PTAL=	pt_conf.c pt_exec.c pt_file.c pt_tcp.c
SRCS=	mount_portal.c conf.c activate.c ${PTAL}
MAN8=	mount_portal.0

.include <bsd.prog.mk>
