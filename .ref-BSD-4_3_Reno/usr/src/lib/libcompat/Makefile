#	@(#)Makefile	5.4 (Berkeley) 6/23/90

LIB=compat
CFLAGS+=-DLIBC_SCCS -DSYSLIBC_SCCS
AINC=	-I${.CURDIR}/../libc/${MACHINE}

.PATH:	${.CURDIR}/4.1/${MACHINE} ${.CURDIR}/4.1 \
	${.CURDIR}/4.3/${MACHINE} ${.CURDIR}/4.3

# compat 4.1 sources
SRCS=	ftime.c getpw.c gtty.c stty.c tell.c times.c utime.c vlimit.c vtimes.c

.if (${MACHINE} == "vax")
SRCS+=	reset.s
.endif

MAN3+=	ftime.0 getpw.0 stty.0 times.0 utime.0 vlimit.0 vtimes.0

MLINKS+=stty.3 gtty.3

# compat 4.3 sources
SRCS+=	ecvt.c gcvt.c lsearch.c rexec.c ruserpass.c sibuf.c sobuf.c strout.c

.if (${MACHINE} == "tahoe")
SRCS+=	nargs.s
.elif (${MACHINE} == "vax")
SRCS+=	nargs.s
.endif

MAN3+=	ecvt.0 lsearch.0 rexec.0

MLINKS+=ecvt.3 fcvt.3 ecvt.3 gcvt.3

.include <bsd.lib.mk>
