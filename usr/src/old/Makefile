#	@(#)Makefile	5.5 (Berkeley) %G%

SUBDIR=	compact crypt

.if   make(clean) || make(cleandir)
SUBDIR+=talk
.elif ${MACHINE} == "vax"
SUBDIR+=talk
.endif

.include <bsd.subdir.mk>
