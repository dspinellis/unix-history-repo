#	@(#)Makefile	5.2 (Berkeley) %G%

LIB=compat
CFLAGS+=-DLIBC_SCCS -DSYSLIBC_SCCS

.PATH:	${.CURDIR}/4.1/${MACHINE} ${.CURDIR}/4.1 \
	${.CURDIR}/4.3/${MACHINE} ${.CURDIR}/4.3

# compat 4.1 sources
SRCS=	ftime.c getpw.c gtty.c rand.c reset.c stty.c tell.c times.c \
	utime.c vlimit.c vtimes.c

MAN3+=	ftime.0 getpw.0 rand.0 stty.0 times.0 utime.0 vlimit.0 vtimes.0

MLINKS+=rand.3 srand.3
MLINKS+=stty.3 gtty.3

# compat 4.3 sources
SRCS+=	ecvt.c gcvt.c lsearch.c rexec.c ruserpass.c sibuf.c sobuf.c strout.c

MAN3+=	ecvt.0 lsearch.0 rexec.0

MLINKS+=ecvt.3 fcvt.3 ecvt.3 gcvt.3

.include <bsd.lib.mk>
