#	@(#)Makefile	5.1 (Berkeley) 6/17/90

.if   (${MACHINE} == "hp300")
.elif (${MACHINE} == "tahoe")
SUBDIR=	c2 ccom f1
.elif (${MACHINE} == "vax")
SUBDIR=	c2 ccom f1 sccom
.endif

.include <bsd.subdir.mk>
