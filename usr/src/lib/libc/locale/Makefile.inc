#	@(#)Makefile.inc	5.1 (Berkeley) %G%

# locale sources
.PATH: ${.CURDIR}/${MACHINE}/locale ${.CURDIR}/locale

SRCS+=	lconv.c localeconv.c setlocale.c
