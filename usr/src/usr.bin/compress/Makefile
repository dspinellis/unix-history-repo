#	@(#)Makefile	5.12 (Berkeley) %G%

PROG=	compress
CFLAGS+=-DBSD4_2 -DSACREDMEM=256000 -DUSERMEM=`cat USERMEM`
DPADD=	${LIBCOMPAT}
LDADD=	-lcompat
LINKS=	${BINDIR}/compress ${BINDIR}/uncompress \
	${BINDIR}/compress ${BINDIR}/zcat
MLINKS=	compress.1 uncompress.1 compress.1 zcat.1

all: USERMEM

# USERMEM may have to be set by hand.  It should contain the amount of
# available user memory in bytes.  Set it to zero, for physical memory
# less than 1 Meg.
USERMEM: ${.CURDIR}/usermem.sh
	sh ${.CURDIR}/usermem.sh > USERMEM

.include <bsd.prog.mk>
