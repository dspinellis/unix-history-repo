#	@(#)Makefile	5.25 (Berkeley) %G%

SUBDIR=	libc libcurses libcompat libdbm libm libmp libplot \
	libresolv librpc/rpc libterm libutil liby

.if	${MACHINE} == "tahoe"
SUBDIR+=csu.tahoe.pcc
.elif	${MACHINE} == "vax"
SUBDIR+=csu.vax.pcc
.else
SUBDIR+=csu.${MACHINE}
.endif

.include <bsd.subdir.mk>
