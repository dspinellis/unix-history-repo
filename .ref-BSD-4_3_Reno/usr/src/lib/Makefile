#	@(#)Makefile	5.20 (Berkeley) 7/3/90

SUBDIR=	libF77 libI77 libc libcurses libcompat libdbm libm libmp \
	libpc libplot libresolv librpc libterm libutil liby

.if ${MACHINE} == "tahoe"
SUBDIR+=csu libU77
.elif ${MACHINE} == "vax"
SUBDIR+=csu libU77
.endif

.include <bsd.subdir.mk>
