#	@(#)Makefile	5.21 (Berkeley) %G%

SUBDIR=	libF77 libI77 libc libcurses libcompat libdbm libm libmp \
	libpc libplot libresolv librpc/rpc libterm libutil liby

.if ${MACHINE} == "tahoe"
SUBDIR+=csu libU77
.elif ${MACHINE} == "vax"
SUBDIR+=csu libU77
.endif

.include <bsd.subdir.mk>
