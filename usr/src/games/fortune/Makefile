#	@(#)Makefile	5.4 (Berkeley) %G%

SUBDIR=	fortune

.ifmake !(install)
SUBDIR+=strfile
.else
SUBDIR+=datfiles
.endif

.include <bsd.subdir.mk>
