#	@(#)Makefile.inc	5.4 (Berkeley) %G%

# compat-43 sources
.PATH: ${.CURDIR}/${MACHINE}/compat-43 ${.CURDIR}/compat-43

SRCS+=	creat.c getwd.c killpg.c regex.c setpgrp.c sigcompat.c

MAN2+=	creat.0 killpg.0 sigblock.0 sigpause.0 sigsetmask.0 sigvec.0
