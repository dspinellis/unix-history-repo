#	@(#)Makefile	7.32 (Berkeley) 6/28/90

# Things that can be turned off to save space:
#	LISPCODE (-l flag, showmatch and lisp options)
#	UCVISUAL (visual \ nonsense on upper case only terminals)
#	CHDIR (the undocumented chdir command)
#	CRYPT includes code to edit encrypted files (the -x option, like ed.)
#	VMUNIX makes ex considerably larger, raising many limits and
#		improving speed and simplicity of maintenance.
#
# If your system expands tabs to 4 spaces you should -DTABS=4 below.
#
PROG=	ex
SUBDIR=	ex3.7recover ex3.7preserve
CFLAGS+=-I. -DTABS=8 -DCRYPT -DLISPCODE -DCHDIR -DUCVISUAL -DVFORK -DVMUNIX \
	-DFASTTAG -DUNIX_SBRK
SRCS=	ex.c ex_addr.c ex_cmds.c ex_cmds2.c ex_cmdsub.c ex_data.c ex_extern.c \
	ex_get.c ex_io.c ex_put.c ex_re.c ex_set.c ex_subr.c ex_tagio.c \
	ex_temp.c ex_tty.c ex_unix.c ex_v.c ex_vadj.c ex_vget.c ex_vmain.c \
	ex_voper.c ex_vops.c ex_vops2.c ex_vops3.c ex_vput.c ex_vwind.c \
	printf.c
BINMODE=1755
DPADD=	strings.o ${LIBTERMCAP}
LDADD=	strings.o -z -ltermcap
MAN1=	ex.0 vi.0
LINKS=	${BINDIR}/ex ${BINDIR}/vi ${BINDIR}/ex ${BINDIR}/view \
	${BINDIR}/ex ${BINDIR}/edit

# There's a boostrap problem here -- ex is necessary to build ex_vars.h.
# The file ex_vars.h.init can be used to build a version of ex to start
# with.
CLEANFILES+=ex_vars.h strings xs.c xs.s x.c strings.o

SHAREDSTRINGS=	sharedstrings

ex_vars.h: ex_data.c
	csh -f ${.CURDIR}/makeoptions ${.CURDIR}/ex_data.c ${CFLAGS:M-[ID]*}

# xstr: hands off!
strings.o: strings
	xstr
	${CC} -c -S xs.c
	ed - < ${.CURDIR}/rofix xs.s
	${AS} -o strings.o xs.s
	rm xs.s

.include <bsd.prog.mk>

${OBJS} ${SUBDIR} .depend: ex_vars.h
${OBJS}: strings
strings:
	touch strings
