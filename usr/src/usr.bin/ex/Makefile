#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.33.1.1 (Berkeley) %G%
#
#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.33.1.1 (Berkeley) %G%
#
#	@(#)Makefile	7.33.1.1 (Berkeley) %G%

# Things that can be turned off to save space:
#	LISPCODE (-l flag, showmatch and lisp options)
#	UCVISUAL (visual \ nonsense on upper case only terminals)
#	CHDIR (the undocumented chdir command)
#	VMUNIX makes ex considerably larger, raising many limits and
#		improving speed and simplicity of maintenance.
#
# If your system expands tabs to 4 spaces you should -DTABS=4 below.
#
PROG=	ex
SUBDIR=	ex3.7recover ex3.7preserve
CFLAGS+=-I. -DTABS=8 -DLISPCODE -DCHDIR -DUCVISUAL -DVFORK -DVMUNIX \
	-DFASTTAG -DUNIX_SBRK
SRCS=	ex.c ex_addr.c ex_cmds.c ex_cmds2.c ex_cmdsub.c ex_data.c ex_extern.c \
	ex_get.c ex_io.c ex_put.c ex_re.c ex_set.c ex_subr.c ex_tagio.c \
	ex_temp.c ex_tty.c ex_unix.c ex_v.c ex_vadj.c ex_vget.c ex_vmain.c \
	ex_voper.c ex_vops.c ex_vops2.c ex_vops3.c ex_vput.c ex_vwind.c \
	printf.c
BINMODE=1755
DPADD=	${LIBTERMCAP}
LDADD=	-z -ltermcap
MAN1=	ex.0 vi.0
LINKS=	${BINDIR}/ex ${BINDIR}/vi ${BINDIR}/ex ${BINDIR}/view \
	${BINDIR}/ex ${BINDIR}/edit

# There's a boostrap problem here -- ex is necessary to build ex_vars.h.
# The file ex_vars.h.init can be used to build a version of ex to start
# with.
CLEANFILES+=ex_vars.h

ex_vars.h: ex_data.c
	csh -f ${.CURDIR}/makeoptions ${.CURDIR}/ex_data.c ${CFLAGS:M-[ID]*}

.include <bsd.prog.mk>

${OBJS} ${SUBDIR} .depend: ex_vars.h
