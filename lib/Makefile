#	@(#)Makefile	5.25.1.1 (Berkeley) 5/7/91

SUBDIR=	csu.${MACHINE} libc libcurses libg++ libm \
	libresolv librpc libtelnet libterm libutil liby

.if exists(libcrypt)
SUBDIR+= libcrypt
.endif

.include <bsd.subdir.mk>
