#	@(#)Makefile	5.11 (Berkeley) %G%

LIB=compat
CFLAGS+=-DLIBC_SCCS -DSYSLIBC_SCCS
AINC=	-I${.CURDIR}/../libc/${MACHINE}

.PATH:	${.CURDIR}/4.1/${MACHINE} ${.CURDIR}/4.1 \
	${.CURDIR}/4.3/${MACHINE} ${.CURDIR}/4.3

# compat 4.1 sources
SRCS=	ftime.c getpw.c gtty.c stty.c tell.c vlimit.c vtimes.c

.if (${MACHINE} == "vax")
SRCS+=	reset.s
.endif

MAN3+=	ftime.0 getpw.0 stty.0 vlimit.0 vtimes.0

MLINKS+=stty.3 gtty.3

# compat 4.3 sources
SRCS+=	ecvt.c gcvt.c lsearch.c regex.c rexec.c ruserpass.c sibuf.c \
	sobuf.c strout.c

.if	(${MACHINE} == "hp300" || ${MACHINE} == "luna68k")
SRCS+=	insque.s remque.s
.elif	(${MACHINE} == "i386")
SRCS+=	insque.c remque.c
.elif	(${MACHINE} == "sparc")
SRCS+=	insque.c remque.c
.elif	(${MACHINE} == "tahoe")
SRCS+=	nargs.s insque.s remque.s
.elif	(${MACHINE} == "vax")
SRCS+=	nargs.s insque.s remque.s
.endif

MAN3+=	ecvt.0 insque.0 lsearch.0 re_comp.0 rexec.0

MLINKS+=ecvt.3 fcvt.3 ecvt.3 gcvt.3
MLINKS+=insque.3 remque.3
MLINKS+=re_comp.3 re_exec.3

.include <bsd.lib.mk>
