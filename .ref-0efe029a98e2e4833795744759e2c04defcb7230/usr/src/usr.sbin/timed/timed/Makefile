#	@(#)Makefile	5.11 (Berkeley) %G%
#
# optional flags are: MEASURE TESTING DEBUG

PROG=	timed
SRCS=	acksend.c candidate.c correct.c master.c networkdelta.c readmsg.c \
	slave.c timed.c byteorder.c measure.c cksum.${MACHINE}.c
DPADD=	${LIBUTIL}
LDADD=	-lutil
MAN8=	timed.0

.include "../../Makefile.inc"
.include <bsd.prog.mk>
