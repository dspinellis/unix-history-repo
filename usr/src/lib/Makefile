#	@(#)Makefile	5.19 (Berkeley) %G%

SUBDIR=	libF77 libI77 libc libcurses libcompat libdbm libm libmp \
	libpc libplot libresolv librpc libterm libutil liby

.if   make(clean) || make(cleandir)
SUBDIR+=libU77
.elif ${MACHINE} == "tahoe"
SUBDIR+=csu libU77
.elif ${MACHINE} == "vax"
SUBDIR+=csu libU77
.endif

.include <bsd.subdir.mk>
