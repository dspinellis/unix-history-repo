#	@(#)Makefile.inc	5.9 (Berkeley) %G%

# compat-43 sources
.PATH: ${.CURDIR}/${MACHINE}/compat-43 ${.CURDIR}/compat-43

SRCS+=	creat.c getwd.c killpg.c setpgrp.c setregid.c setreuid.c \
	setrgid.c setruid.c sigcompat.c

MAN2+=	creat.0 killpg.0 setregid.0 setreuid.0 setruid.0 sigblock.0 \
	sigpause.0 sigsetmask.0 sigvec.0

MLINKS+=setruid.3 setrgid.3
